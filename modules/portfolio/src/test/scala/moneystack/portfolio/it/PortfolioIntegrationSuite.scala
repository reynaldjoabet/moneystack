package moneystack.portfolio.it

import moneystack.domain.*
import moneystack.portfolio.PortfolioModule
import moneystack.portfolio.domain.*
import moneystack.portfolio.service.*
import moneystack.portfolio.service.PortfolioService.{NewAccount, NewActivity, NewAsset, NewLimit}

import cats.effect.*
import skunk.Session

import java.time.LocalDate
import java.util.UUID

class PortfolioIntegrationSuite extends moneystack.testkit.PostgresSuite[PortfolioModule[IO]] {

  def schemas: List[String] = List("db/portfolio.sql")

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, PortfolioModule[IO]] =
    Resource.eval(IO(PortfolioModule.make[IO](pool, CurrencyCode.USD)))

  private def pf                  = env.portfolio
  private val usd                 = CurrencyCode.USD
  private val eur                 = CurrencyCode.EUR
  private val me                  = UserId.assume(UUID.randomUUID())
  private val you                 = UserId.assume(UUID.randomUUID())
  private def d(m: Int, day: Int) = LocalDate.of(2026, m, day)

  private def account(owner: UserId, name: String, ccy: CurrencyCode = usd, tracking: Tracking = Tracking.Activities) =
    pf.createAccount(owner, NewAccount(Title.applyUnsafe(name), AccountKind.Brokerage, Some(tracking), ccy, None))

  private def asset(sym: String, kind: AssetKind, ccy: CurrencyCode) =
    ok(pf.createAsset(NewAsset(Symbol.applyUnsafe(sym), Title.applyUnsafe(sym), kind, ccy)))

  private def activity(
      acct: InvestmentAccount,
      kind: ActivityType,
      date: LocalDate,
      asset: Option[Asset] = None,
      qty: Option[String] = None,
      price: Option[String] = None,
      amount: Option[String] = None,
      fee: Option[String] = None,
      ccy: CurrencyCode = usd
  ) =
    NewActivity(
      acct.id,
      asset.map(_.id),
      None,
      kind,
      date,
      qty.map(BigDecimal(_)),
      price.map(BigDecimal(_)),
      amount.map(BigDecimal(_)),
      fee.map(BigDecimal(_)),
      asset.fold(ccy)(_.currency),
      None,
      None,
      None
    )

  test("a brokerage account end to end: activities, holdings, performance, income, realised gains, allocation") {
    for {
      acct <- account(me, "Brokerage")
      aapl <- asset("AAPL", AssetKind.Equity, usd)
      vwce <- asset("VWCE.DE", AssetKind.Etf, eur)
      _    <- ok(pf.classify(
        vwce.id,
        Taxonomy.Region,
        List(Title.applyUnsafe("US") -> Bps.assume(6000), Title.applyUnsafe("Europe") -> Bps.assume(4000))
      ))
      _ <- ok(pf.addFxRates(List(FxRate(eur, usd, d(1, 1), BigDecimal("1.10")))))
      _ <- ok(pf.addQuotes(List(
        Quote(aapl.id, d(1, 2), BigDecimal(150)),
        Quote(aapl.id, d(1, 31), BigDecimal(180)),
        Quote(vwce.id, d(1, 3), BigDecimal(100)),
        Quote(vwce.id, d(1, 31), BigDecimal(110))
      )))
      _ <- ok(pf.addActivity(me, activity(acct, ActivityType.Deposit, d(1, 1), amount = Some("10000"))))
      _ <- ok(pf.addActivity(me, activity(acct, ActivityType.Buy, d(1, 2), Some(aapl), Some("20"), Some("150"), fee = Some("1"))))
      _ <- ok(pf.addActivity(me, activity(acct, ActivityType.Buy, d(1, 3), Some(vwce), Some("30"), Some("100"))))
      _ <- ok(pf.addActivity(me, activity(acct, ActivityType.Sell, d(1, 20), Some(aapl), Some("5"), Some("170"))))
      _ <- ok(pf.addActivity(me, activity(acct, ActivityType.Dividend, d(1, 25), Some(aapl), amount = Some("12"))))
      _ <- failsWith(pf.addActivity(me, activity(acct, ActivityType.Sell, d(1, 26), Some(aapl), Some("100"), Some("170")))) {
        case PortfolioError.InsufficientQuantity(_, _, _, _) =>
      }
      hold <- ok(pf.holdings(me, None, d(1, 31), usd))
      perf <- ok(pf.performance(me, None, d(1, 1), d(1, 31), usd))
      inc  <- ok(pf.income(me, None, d(1, 1), d(1, 31), usd))
      real <- ok(pf.realized(me, None, d(1, 1), d(1, 31), usd))
      reg  <- ok(pf.allocation(me, "region", d(1, 31), usd))
    } yield {
      val aaplRow = hold.positions.find(_.assetId == aapl.id).get
      assertEquals(aaplRow.quantity, BigDecimal(15))
      assertEquals(aaplRow.marketValueBase, BigDecimal("2700.00"))
      val vwceRow = hold.positions.find(_.assetId == vwce.id).get
      assertEquals(vwceRow.marketValueBase, BigDecimal("3630.00")) // 30 x 110 x 1.10
      assertEquals(vwceRow.costBase, BigDecimal("3300.00"))
      // cash: 10,000 - 3,001 + 850 + 12 = 7,861 USD, and -3,000 EUR (= -3,300 USD)
      assertEquals(hold.total, BigDecimal("10891.00"))
      assertEquals(hold.cash, BigDecimal("4561.00"))
      val r = perf.returns.get
      assertEquals(r.netFlows, BigDecimal("0.00")) // the deposit is the opening value
      assert(r.twr > 0, r.toString)
      assertEquals(inc.dividends, BigDecimal("12.00"))
      assertEquals(real.realizedBase, BigDecimal("99.75")) // 850 - 5/20 of 3,001
      assertEquals(reg.find(_.category == "US").map(_.value), Some(BigDecimal("2178.00")))
    }
  }

  test("edits and deletes are replayed, so a later sale can't lose its shares") {
    for {
      acct <- account(me, "Edits")
      msft <- asset("MSFT", AssetKind.Equity, usd)
      buy  <- ok(pf.addActivity(me, activity(acct, ActivityType.Buy, d(2, 1), Some(msft), Some("10"), Some("400"))))
      _    <- ok(pf.addActivity(me, activity(acct, ActivityType.Sell, d(2, 5), Some(msft), Some("8"), Some("410"))))
      _    <- failsWith(pf.deleteActivity(me, buy.id)) { case PortfolioError.InsufficientQuantity(_, _, _, _) => }
      _    <- failsWith(pf.updateActivity(me, buy.id, activity(acct, ActivityType.Buy, d(2, 1), Some(msft), Some("5"), Some("400")))) {
        case PortfolioError.InsufficientQuantity(_, _, _, _) =>
      }
      upd  <- ok(pf.updateActivity(me, buy.id, activity(acct, ActivityType.Buy, d(2, 1), Some(msft), Some("12"), Some("390"))))
      hold <- ok(pf.holdings(me, Some(List(acct.id)), d(2, 28), usd))
    } yield {
      assertEquals(upd.quantity, Some(BigDecimal(12)))
      assertEquals(hold.positions.map(_.quantity), List(BigDecimal(4)))
    }
  }

  test("CSV import creates unknown assets and is idempotent") {
    val csv =
      """date,type,symbol,quantity,unit_price,amount,currency
        |2026-03-01,deposit,,,,5000,USD
        |2026-03-02,buy,NVDA,10,120,,USD
        |2026-03-03,dividend,NVDA,,,4,USD
        |""".stripMargin
    for {
      acct   <- account(me, "Imported")
      dry    <- ok(pf.importCsv(me, acct.id, csv, dryRun = true))
      first  <- ok(pf.importCsv(me, acct.id, csv, dryRun = false))
      second <- ok(pf.importCsv(me, acct.id, csv, dryRun = false))
      acts   <- pf.activities(me, Some(acct.id), d(1, 1), d(12, 31))
      _      <- failsWith(pf.importCsv(me, acct.id, "date,type\n2026-01-01,buy", dryRun = false)) { case PortfolioError.ImportFailed(_) => }
    } yield {
      assertEquals((dry.imported, dry.createdAssets.map(s => s.value: String)), (3, List("NVDA")))
      assertEquals(first.imported, 3)
      assertEquals((second.imported, second.duplicates), (0, 3))
      assertEquals(acts.size, 3)
    }
  }

  test("targets, rebalancing and contribution limits") {
    val saver = UserId.assume(UUID.randomUUID()) // own owner: rebalancing reads every holding
    for {
      acct <- account(saver, "ISA")
      bond <- asset("AGG", AssetKind.Bond, usd)
      _    <- ok(pf.addActivity(saver, activity(acct, ActivityType.Deposit, d(4, 1), amount = Some("1000"))))
      _    <- ok(pf.addActivity(saver, activity(acct, ActivityType.Buy, d(4, 2), Some(bond), Some("10"), Some("50"))))
      _    <- failsWith(pf.setTargets(saver, Taxonomy.applyUnsafe("asset_kind"), List(Title.applyUnsafe("bond") -> Bps.assume(5000)))) {
        case PortfolioError.InvalidTargets(_) =>
      }
      _ <- ok(pf.setTargets(
        saver,
        Taxonomy.applyUnsafe("asset_kind"),
        List(Title.applyUnsafe("bond") -> Bps.assume(5000), Title.applyUnsafe("equity") -> Bps.assume(5000))
      ))
      plan <- ok(pf.rebalance(saver, Taxonomy.applyUnsafe("asset_kind"), BigDecimal(0), RebalanceMode.Full, d(4, 30), usd))
      lim  <- ok(pf.createLimit(saver, NewLimit(Title.applyUnsafe("ISA 2026"), 2026, BigDecimal(20000), usd, List(acct.id), None, None)))
      prog <- ok(pf.limits(saver, usd))
    } yield {
      assertEquals(
        plan.map(r => r.category -> r.trade).toMap,
        Map("bond" -> BigDecimal("0.00"), "equity" -> BigDecimal("500.00"), "Cash" -> BigDecimal("-500.00"))
      )
      val p = prog.find(_.limit.id == lim.id).get
      assertEquals(p.contributed, BigDecimal("1000.00"))
      assertEquals(p.remaining, BigDecimal("19000.00"))
    }
  }

  test("net worth adds balance accounts, property and vehicles and subtracts loans") {
    val nw = env.netWorth
    for {
      savings <- account(me, "Savings book", tracking = Tracking.Balances)
      _       <- ok(pf.recordBalance(me, BalanceSnapshot(savings.id, d(5, 1), BigDecimal(8000), BigDecimal(7500))))
      _       <- failsWith(pf.addActivity(me, activity(savings, ActivityType.Deposit, d(5, 2), amount = Some("1")))) {
        case PortfolioError.WrongTracking(_, _) =>
      }
      flat <- ok(nw.createAltAsset(
        me,
        NetWorthService.NewAltAsset(
          AltAssetKind.RealEstate,
          Title.applyUnsafe("Flat"),
          usd,
          BigDecimal(300_000),
          d(1, 1),
          Some(Bps.assume(5000)),
          None,
          None,
          None,
          None
        )
      ))
      _ <- ok(nw.addValuation(me, AltValuation(flat.id, d(5, 1), BigDecimal(320_000), Title.applyUnsafe("estimate"))))
      _ <- ok(nw.createAltAsset(
        me,
        NetWorthService.NewAltAsset(
          AltAssetKind.Vehicle,
          Title.applyUnsafe("Car"),
          usd,
          BigDecimal(20_000),
          d(1, 1),
          None,
          None,
          Some(VehicleClass.Sedan),
          None,
          None
        )
      ))
      loan <- ok(nw.createLiability(
        me,
        NetWorthService.NewLiability(
          LiabilityKind.Mortgage,
          Title.applyUnsafe("Mortgage"),
          usd,
          BigDecimal(150_000),
          BigDecimal("0.04"),
          300,
          d(1, 1),
          None,
          Some(flat.id)
        )
      ))
      sched <- ok(nw.schedule(me, loan.id))
      worth <- ok(nw.netWorth(me, d(5, 31), usd))
      hist  <- ok(nw.history(me, d(1, 1), d(5, 31), usd))
    } yield {
      assertEquals(worth.accounts.find(_.id == savings.id.value).map(_.value), Some(BigDecimal("8000.00")))
      assertEquals(worth.assets.find(_.id == flat.id.value).map(_.value), Some(BigDecimal("160000.00"))) // half of 320k
      assert(worth.assets.find(_.name.value == "Car").exists(_.value < 20_000))
      val owed = worth.liabilities.head.value
      assert(owed < 150_000 && owed > 145_000, owed.toString)
      assertEquals(worth.netWorth, worth.totalAssets - worth.totalLiabilities)
      assert(sched.rows.size >= 299 && sched.rows.size <= 301, sched.rows.size.toString)
      assertEquals(hist.last.date, d(5, 31))
      assertEquals(hist.last.netWorth, worth.netWorth)
    }
  }

  test("goals track funded account value and project it") {
    for {
      acct <- account(me, "Goal fund")
      _    <- ok(pf.addActivity(me, activity(acct, ActivityType.Deposit, d(6, 1), amount = Some("4000"))))
      goal <- ok(env.planning.createGoal(
        me,
        PlanningService.NewGoal(
          Title.applyUnsafe("House deposit"),
          BigDecimal(20_000),
          usd,
          Some(LocalDate.of(2028, 6, 1)),
          Some(BigDecimal(500)),
          Some(BigDecimal("0.04")),
          List(GoalFunding(acct.id, Bps.assume(5000)))
        )
      ))
      all <- ok(env.planning.goals(me, d(6, 30)))
    } yield {
      val g = all.find(_.goal.id == goal.id).get
      assertEquals(g.currentValue, BigDecimal("2000.00"))
      assert(Set("on_track", "at_risk", "off_track").contains(g.overview.health))
    }
  }

  test("owners are isolated") {
    for {
      mine   <- account(me, "Private")
      theirs <- pf.accounts(you)
      _      <- failsWith(pf.addActivity(you, activity(mine, ActivityType.Deposit, d(7, 1), amount = Some("1")))) {
        case PortfolioError.AccountNotFound(_) =>
      }
      _    <- failsWith(pf.holdings(you, Some(List(mine.id)), d(7, 1), usd)) { case PortfolioError.AccountNotFound(_) => }
      gone <- pf.setAccountActive(you, mine.id, active = false)
    } yield {
      assertEquals(theirs, Nil)
      assertEquals(gone, None)
    }
  }
}
