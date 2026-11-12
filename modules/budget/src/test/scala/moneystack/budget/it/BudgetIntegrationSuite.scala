package moneystack.budget.it

import moneystack.domain.*
import moneystack.budget.BudgetModule
import moneystack.budget.domain.*
import moneystack.budget.service.*
import moneystack.budget.service.MoneyService.{NewCategory, NewPayee, NewRule, NewTransaction, NewWallet}
import moneystack.platform.FxQuote

import cats.effect.*
import skunk.Session
import skunk.implicits.*

import java.time.{LocalDate, YearMonth}
import java.util.UUID

class BudgetIntegrationSuite extends moneystack.testkit.PostgresSuite[BudgetModule[IO]] {

  def migrations: List[String] = List(BudgetModule.migrations)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, BudgetModule[IO]] =
    Resource.eval(IO(BudgetModule.make[IO](pool, CurrencyCode.USD)))

  private def money               = env.money
  private def plans               = env.plans
  private val usd                 = CurrencyCode.USD
  private def d(m: Int, day: Int) = LocalDate.of(2026, m, day)
  private def t(s: String)        = Title.applyUnsafe(s)

  private def newUser = UserId.assume(UUID.randomUUID())

  private def wallet(owner: UserId, name: String, ccy: CurrencyCode = usd, opening: Long = 0L, kind: WalletKind = WalletKind.Checking) =
    money.createWallet(owner, NewWallet(t(name), kind, ccy, Some(opening)))

  private def category(owner: UserId, name: String) = money.categories(owner).map(_.find(c => (c.name.value: String) == name).get)

  private def txn(
      w: Wallet,
      kind: TxKind,
      amount: Long,
      date: LocalDate,
      title: String,
      cat: Option[CategoryId] = None,
      splits: Option[List[Split]] = None,
      to: Option[Wallet] = None,
      toAmount: Option[Long] = None,
      refundOf: Option[TransactionId] = None,
      objective: Option[ObjectiveId] = None
  ) =
    NewTransaction(w.id, kind, amount, date, t(title), None, cat, None, None, splits, to.map(_.id), toAmount, refundOf, objective, None)

  test("wallets seed default categories and can be shared read-only") {
    val (alice, bob) = (newUser, newUser)
    for {
      w    <- wallet(alice, "Joint", opening = 50_000)
      cats <- money.categories(alice)
      _    <- ok(money.shareWallet(alice, w.id, bob))
      _    <- failsWith(money.shareWallet(bob, w.id, alice)) { case BudgetError.WalletNotFound(_) => }
      seen <- money.wallets(bob)
      _    <- failsWith(money.addTransaction(bob, txn(w, TxKind.Expense, 100, d(1, 2), "sneaky"))) { case BudgetError.WalletNotFound(_) => }
      bal  <- ok(env.insights.balances(bob, d(1, 31), usd))
      _    <- ok(money.unshareWallet(alice, w.id, bob))
      after <- money.wallets(bob)
    } yield {
      assert(cats.exists(c => (c.name.value: String) == "Groceries" && c.kind == CategoryKind.Expense))
      assert(cats.exists(c => (c.name.value: String) == "Salary" && c.kind == CategoryKind.Income))
      assertEquals(seen.map(v => (v.wallet.id, v.shared)), List(w.id -> true))
      assertEquals(bal.netWorthMinor, 50_000L)
      assertEquals(after, Nil)
    }
  }

  test("transactions: payee defaults, rules, splits, transfers and refunds") {
    val me = newUser
    for {
      checking  <- wallet(me, "Checking", opening = 200_000)
      card      <- wallet(me, "Card", kind = WalletKind.CreditCard)
      groceries <- category(me, "Groceries")
      dining    <- category(me, "Dining")
      coffee    <- ok(money.createCategory(me, NewCategory(t("Coffee"), CategoryKind.Expense, Some(dining.id), Some("#6D4C41"))))
      _ <- failsWith(money.createCategory(me, NewCategory(t("Coffee"), CategoryKind.Expense, None, None))) { case BudgetError.Invalid(_) =>
      }
      tesco <- ok(money.createPayee(me, NewPayee(t("Tesco"), Some(List("TESCO STORES")), Some(groceries.id))))
      _     <- ok(money.createRule(
        me,
        NewRule(
          t("Coffee shops"),
          None,
          None,
          List(Condition.Text(TextField.Title, TextOp.ContainsAny, List("starbucks", "pret"))),
          List(Action.SetCategory(coffee.id))
        )
      ))
      shop  <- ok(money.addTransaction(me, txn(checking, TxKind.Expense, 8_000, d(2, 1), "TESCO STORES 3297")))
      latte <- ok(money.addTransaction(me, txn(checking, TxKind.Expense, 450, d(2, 2), "Starbucks Victoria")))
      split <- ok(money.addTransaction(
        me,
        txn(
          card,
          TxKind.Expense,
          6_000,
          d(2, 3),
          "Market",
          splits = Some(List(Split(groceries.id, 4_000, None), Split(dining.id, 2_000, None)))
        )
      ))
      _ <- failsWith(money.addTransaction(
        me,
        txn(
          card,
          TxKind.Expense,
          6_000,
          d(2, 3),
          "Bad split",
          splits = Some(List(Split(groceries.id, 1_000, None)))
        )
      )) { case BudgetError.Invalid(_) => }
      _ <- ok(money.addTransaction(me, txn(checking, TxKind.Transfer, 6_000, d(2, 4), "Pay card", to = Some(card))))
      _ <- failsWith(money.addTransaction(me, txn(checking, TxKind.Transfer, 1, d(2, 4), "Loop", to = Some(checking)))) {
        case BudgetError.Invalid(_) =>
      }
      _    <- ok(money.addTransaction(me, txn(checking, TxKind.Income, 1_000, d(2, 5), "Tesco refund", refundOf = Some(shop.id))))
      bals <- ok(env.insights.balances(me, d(2, 28), usd))
      cats <- ok(env.insights.categories(me, Period(d(2, 1), d(2, 28)), CategoryKind.Expense, usd, None))
    } yield {
      assertEquals((shop.payeeId, shop.categoryId, shop.categorySource), (Some(tesco.id), Some(groceries.id), CategorySource.Payee))
      assertEquals((latte.categoryId, latte.categorySource), (Some(coffee.id), CategorySource.Rule))
      assertEquals(split.splits.size, 2)
      val b = bals.wallets.map(x => x.walletId -> x.balanceMinor).toMap
      assertEquals(b(checking.id), 200_000L - 8_000 - 450 - 6_000 + 1_000)
      assertEquals(b(card.id), 0L)
      assertEquals(cats.find(_.name == "Groceries").map(_.amountMinor), Some(8_000L + 4_000 - 1_000))
      assertEquals(cats.find(_.name == "Dining").map(_.amountMinor), Some(2_000L + 450)) // coffee rolls into dining
    }
  }

  test("rules can be previewed and applied to history without touching manual categories") {
    val me = newUser
    for {
      w      <- wallet(me, "Main")
      travel <- category(me, "Transport")
      fees   <- category(me, "Fees")
      a      <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 1_500, d(3, 1), "UBER TRIP")))
      b      <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 2_500, d(3, 2), "Uber Trip", cat = Some(fees.id))))
      rule   <- ok(money.createRule(
        me,
        NewRule(
          t("Uber"),
          None,
          None,
          List(Condition.Text(TextField.Title, TextOp.StartsWithAny, List("uber"))),
          List(Action.SetCategory(travel.id))
        )
      ))
      dry   <- ok(money.runRuleOnHistory(me, rule.id, apply = false))
      run   <- ok(money.runRuleOnHistory(me, rule.id, apply = true))
      txs   <- money.transactions(me, MoneyService.Filter(None, None, Some("uber"), d(1, 1), d(12, 31), 50))
      rules <- money.rules(me)
    } yield {
      assertEquals((dry.matched, dry.changed, dry.applied), (2, 1, false)) // both match; only the uncategorised one changes
      assertEquals(run.changed, 1)
      assertEquals(txs.find(_.id == a.id).flatMap(_.categoryId), Some(travel.id))
      assertEquals(txs.find(_.id == b.id).flatMap(_.categoryId), Some(fees.id)) // chosen by hand, left alone
      assertEquals(rules.head.matchCount, 1L)
    }
  }

  test("schedules: upcoming, pay, skip, idempotent auto-pay and subscription cost") {
    val me = newUser
    for {
      w       <- wallet(me, "Bills")
      subs    <- category(me, "Subscriptions")
      netflix <- ok(plans.createSchedule(
        me,
        PlanService.NewSchedule(
          ScheduleKind.Subscription,
          t("Netflix"),
          w.id,
          TxKind.Expense,
          1_599,
          Some(subs.id),
          None,
          None,
          Frequency.Monthly,
          None,
          d(1, 31),
          None,
          None,
          Some(true),
          Some(5)
        )
      ))
      gym <- ok(plans.createSchedule(
        me,
        PlanService.NewSchedule(
          ScheduleKind.Subscription,
          t("Gym"),
          w.id,
          TxKind.Expense,
          12_000,
          None,
          None,
          None,
          Frequency.Yearly,
          None,
          d(6, 1),
          None,
          None,
          Some(false),
          None
        )
      ))
      upcoming <- plans.upcoming(me, d(3, 25), 10)
      first    <- plans.processDue(me, d(3, 31))
      again    <- plans.processDue(me, d(3, 31))
      paidGym  <- ok(plans.pay(me, gym.id, Some(d(5, 30)), Some(11_000)))
      skipped  <- ok(plans.skip(me, gym.id))
      views    <- plans.schedules(me)
      cost     <- ok(plans.subscriptionCost(me, usd, d(4, 1)))
      txs      <- money.transactions(me, MoneyService.Filter(None, None, None, d(1, 1), d(12, 31), 50))
    } yield {
      assertEquals(
        upcoming.map(u => (u.title.value: String, u.due, u.overdue)),
        List(("Netflix", d(1, 31), true), ("Netflix", d(2, 28), true), ("Netflix", d(3, 31), false))
      )
      assertEquals((first, again), (3, 0))
      assertEquals(txs.count(_.scheduleId.contains(netflix.id)), 3)
      assertEquals(txs.filter(_.scheduleId.contains(netflix.id)).map(_.categorySource).distinct, List(CategorySource.Schedule))
      assertEquals((paidGym.amountMinor, paidGym.dueDate), (11_000L, Some(d(6, 1))))
      assertEquals(skipped, Some(LocalDate.of(2028, 6, 1)))
      assertEquals(views.find(_.schedule.id == netflix.id).flatMap(_.nextDue), Some(d(4, 30)))
      assertEquals(cost.monthlyMinor, 1_599L + 1_000)
    }
  }

  test("budgets: category filters, limits, rollover and history") {
    val me = newUser
    for {
      w      <- wallet(me, "Spending")
      food   <- category(me, "Groceries")
      fun    <- category(me, "Entertainment")
      budget <- ok(plans.createBudget(
        me,
        PlanService.NewBudget(
          t("Food"),
          30_000,
          usd,
          Recurrence.Monthly,
          None,
          d(1, 1),
          None,
          None,
          Some(List(food.id)),
          None,
          None,
          Some(true),
          Some(List(CategoryLimit(food.id, 30_000))),
          Some(true)
        )
      ))
      _     <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 20_000, d(1, 10), "Jan shop", cat = Some(food.id))))
      _     <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 25_000, d(2, 10), "Feb shop", cat = Some(food.id))))
      _     <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 99_000, d(2, 11), "Concert", cat = Some(fun.id))))
      views <- ok(plans.budgets(me, d(2, 14)))
      hist  <- ok(plans.budgetHistory(me, budget.id, d(2, 14), 3))
      _     <- failsWith(plans.createBudget(
        me,
        PlanService.NewBudget(
          t("Bad"),
          1,
          usd,
          Recurrence.Custom,
          None,
          d(1, 1),
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      )) { case BudgetError.Invalid(_) => }
    } yield {
      val st = views.find(_.budget.id == budget.id).flatMap(_.status).get
      assertEquals((st.spentMinor, st.rolloverMinor, st.availableMinor, st.remainingMinor), (25_000L, 10_000L, 40_000L, 15_000L))
      assertEquals(hist.map(_.spentMinor), List(25_000L, 20_000L))
    }
  }

  test("objectives, CSV import/export, FX and insights") {
    val me = newUser
    for {
      w    <- wallet(me, "Main", opening = 10_000)
      eurW <- wallet(me, "Euro", CurrencyCode.EUR)
      _    <- ok(money.addRates(List(FxQuote(CurrencyCode.EUR, usd, d(1, 1), BigDecimal("1.20")))))
      trip <- ok(plans.createObjective(me, PlanService.NewObjective(ObjectiveKind.Goal, t("Trip"), 100_000, usd, Some(d(12, 1)), None)))
      loan <-
        ok(plans.createObjective(me, PlanService.NewObjective(ObjectiveKind.Lent, t("Loan to Sam"), 50_000, usd, None, Some(t("Sam")))))
      _    <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 20_000, d(4, 1), "To trip fund", objective = Some(trip.id))))
      _    <- ok(money.addTransaction(me, txn(eurW, TxKind.Expense, 10_000, d(4, 2), "Trip deposit (EUR)", objective = Some(trip.id))))
      _    <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 50_000, d(4, 3), "Lent to Sam", objective = Some(loan.id))))
      _    <- ok(money.addTransaction(me, txn(w, TxKind.Income, 20_000, d(4, 20), "Sam repaid", objective = Some(loan.id))))
      objs <- ok(plans.objectives(me))
      csv =
        "date,description,amount,category\n2026-05-01,Salary May,3000.00,Salary\n2026-05-02,Corner shop,-12.30,Groceries\n2026-05-03,Pottery class,-40,Hobbies\n"
      dry    <- ok(money.importCsv(me, w.id, csv, dryRun = true))
      imp    <- ok(money.importCsv(me, w.id, csv, dryRun = false))
      again  <- ok(money.importCsv(me, w.id, csv, dryRun = false))
      out    <- money.exportCsv(me, d(5, 1), d(5, 31))
      sum    <- ok(env.insights.summary(me, Period(d(5, 1), d(5, 31)), usd, None))
      flow   <- ok(env.insights.cashflow(me, Period(d(5, 1), d(5, 31)), usd))
      trend  <- ok(env.insights.trend(me, Period(d(4, 1), d(5, 31)), Bucket.Month, usd, None))
      recap  <- ok(env.insights.recap(me, YearMonth.of(2026, 5), usd))
      hist   <- ok(env.insights.balanceHistory(me, Period(d(3, 1), d(5, 31)), usd))
      streak <- env.insights.streak(me, d(5, 3))
    } yield {
      val tripP = objs.find(_.objective.id == trip.id).get
      assertEquals(tripP.amountMinor, 20_000L + 12_000) // EUR 100 at 1.20
      val loanP = objs.find(_.objective.id == loan.id).get
      assertEquals((loanP.amountMinor, loanP.pct), (30_000L, BigDecimal("40.00")))
      assertEquals((dry.imported, dry.createdCategories), (3, List("Hobbies")))
      assertEquals((imp.imported, again.imported, again.duplicates), (3, 0, 3))
      assertEquals(out.linesIterator.size, 4)
      assertEquals((sum.incomeMinor, sum.expenseMinor), (300_000L, 5_230L))
      assert(flow.contains(FlowLink("Income", "Savings", 294_770L)))
      assertEquals(trend.map(_.expenseMinor), List(20_000L + 12_000 + 50_000, 5_230L))
      assertEquals(recap.summary.expenseMinor, 5_230L)
      assertEquals(hist.last._2, 10_000L - 20_000 - 12_000 - 50_000 + 20_000 + 300_000 - 5_230)
      assertEquals(streak.current, 3)
    }
  }

  test("users can't touch each other's transactions or schedules") {
    val (me, them) = (newUser, newUser)
    for {
      w  <- wallet(me, "Mine")
      tx <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 100, d(6, 1), "private")))
      _  <- failsWith(money.deleteTransaction(them, tx.id)) { case BudgetError.TransactionNotFound(_) => }
      _  <- failsWith(money.updateTransaction(them, tx.id, txn(w, TxKind.Expense, 1, d(6, 1), "x"))) {
        case BudgetError.WalletNotFound(_) | BudgetError.TransactionNotFound(_) =>
      }
      vis <- money.transactions(them, MoneyService.Filter(None, None, None, d(1, 1), d(12, 31), 50))
      _   <- ok(money.deleteTransaction(me, tx.id))
    } yield assertEquals(vis, Nil)
  }

  test("a report for one period keeps the category of an older purchase it refunds") {
    val me = newUser
    for {
      w    <- wallet(me, "Checking")
      food <- category(me, "Groceries")
      old  <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 5_000, d(1, 10), "Shop", cat = Some(food.id))))
      _    <- ok(money.addTransaction(me, txn(w, TxKind.Income, 1_000, d(3, 5), "Refund", refundOf = Some(old.id))))
      march = Period(d(3, 1), d(3, 31))
      sum  <- ok(env.insights.summary(me, march, usd, None))
      cats <- ok(env.insights.categories(me, march, CategoryKind.Expense, usd, None))
      rec  <- ok(env.insights.recap(me, YearMonth.of(2026, 2), usd))
    } yield {
      assertEquals(sum.expenseMinor, -1_000L, "only the refund falls in March")
      assertEquals(sum.transactions, 1)
      assertEquals(cats.map(c => c.name -> c.amountMinor), List("Groceries" -> -1_000L))
      assertEquals(rec.previousExpenseMinor, 5_000L, "a recap reads the month before as well")
    }
  }

  test("balances summed in SQL match the same sum over the transactions") {
    val me = newUser
    for {
      a    <- wallet(me, "A", opening = 1_000L)
      b    <- wallet(me, "B", opening = 50L)
      _    <- ok(money.addTransaction(me, txn(a, TxKind.Income, 700, d(1, 2), "In")))
      _    <- ok(money.addTransaction(me, txn(a, TxKind.Expense, 300, d(1, 3), "Out")))
      _    <- ok(money.addTransaction(me, txn(a, TxKind.Transfer, 200, d(1, 4), "Same", to = Some(b))))
      _    <- ok(money.addTransaction(me, txn(b, TxKind.Transfer, 90, d(1, 5), "Different", to = Some(a), toAmount = Some(80L))))
      _    <- ok(money.addTransaction(me, txn(b, TxKind.Expense, 5, d(2, 1), "Later")))
      all  <- moneystack.budget.db.Transactions.make[IO](sessions).forWallets(List(a.id, b.id))
      jan  <- ok(env.insights.balances(me, d(1, 31), usd))
      feb  <- ok(env.insights.balances(me, d(2, 28), usd))
      hist <- ok(env.insights.balanceHistory(me, Period(d(1, 1), d(2, 10)), usd))
    } yield {
      assertEquals(hist, List(d(1, 31) -> 1_440L, d(2, 10) -> 1_435L), "month ends, then the period's last day")
      def expected(asOf: LocalDate) =
        Reports.balances(List(a, b), all, asOf, moneystack.platform.FxTable.unity, usd).toOption.get.map(x =>
          x.walletId -> x.balanceMinor
        ).toSet
      assertEquals(jan.wallets.map(x => x.walletId -> x.balanceMinor).toSet, expected(d(1, 31)))
      assertEquals(feb.wallets.map(x => x.walletId -> x.balanceMinor).toSet, expected(d(2, 28)))
      assertEquals(jan.wallets.map(x => x.walletId -> x.balanceMinor).toSet, Set(a.id -> 1_280L, b.id -> 160L))
    }
  }

  test("the streak counts only days the user recorded something") {
    val (me, friend) = (newUser, newUser)
    for {
      w <- wallet(me, "Shared")
      _ <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 1, d(4, 9), "One")))
      _ <- ok(money.addTransaction(me, txn(w, TxKind.Expense, 1, d(4, 10), "Two")))
      s <- env.insights.streak(me, d(4, 10))
      n <- env.insights.streak(friend, d(4, 10))
    } yield {
      assertEquals(s, Streak(2, 2, true))
      assertEquals(n, Streak(0, 0, false))
    }
  }

  test("a wallet with more transactions than Postgres allows bind parameters still loads") {
    val owner = newUser
    for {
      w <- wallet(owner, "Busy")
      // 70,000 rows: past the 65,535 parameters a query may bind, which listing every id would need.
      _ <- sessions.use(_.execute(sql"""
             INSERT INTO budget.transactions (id, owner_id, wallet_id, kind, amount_minor, tx_date, title, created_by)
             SELECT gen_random_uuid(), '#${owner.value.toString}', '#${w.id.value.toString}', 'expense', 1,
                    DATE '2026-03-01' + (g % 28), 'bulk', '#${owner.value.toString}'
             FROM generate_series(1, 70000) g""".command))
      all <- moneystack.budget.db.Transactions.make[IO](sessions).forWallets(List(w.id))
      day <- money.transactions(owner, MoneyService.Filter(Some(w.id), None, None, d(3, 2), d(3, 2), 10))
    } yield {
      assertEquals(all.size, 70000)
      assertEquals(day.size, 10)
      assert(day.forall(_.date == d(3, 2)))
    }
  }
}
