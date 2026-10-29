package moneystack.portfolio.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

class EngineSuite extends munit.FunSuite {

  private val usd  = CurrencyCode.USD
  private val eur  = CurrencyCode.EUR
  private val acct = InvestmentAccountId.assume(UUID.randomUUID())
  private val other = InvestmentAccountId.assume(UUID.randomUUID())

  private def asset(sym: String, ccy: CurrencyCode) =
    Asset(AssetId.assume(UUID.randomUUID()), Symbol.applyUnsafe(sym), Title.applyUnsafe(sym), AssetKind.Equity, ccy, Instant.EPOCH)

  private val aapl = asset("AAPL", usd)
  private val sap  = asset("SAP.DE", eur)

  private def ctx(fx: List[FxRate] = Nil, quotes: List[Quote] = Nil) =
    Engine.Context(List(aapl, sap).map(a => a.id -> a).toMap, PriceTable(quotes), FxTable(fx), usd)

  private var seq = 0L
  private def act(kind: ActivityType, day: Int, asset: Option[Asset] = None, qty: String = null, price: String = null,
                  amount: String = null, fee: String = "0", ccy: CurrencyCode = usd, external: Boolean = false,
                  counter: Option[InvestmentAccountId] = None): Activity = {
    seq += 1
    Activity(ActivityId.assume(UUID.randomUUID()), acct, asset.map(_.id), kind, LocalDate.of(2026, 1, day),
             Option(qty).map(BigDecimal(_)), Option(price).map(BigDecimal(_)), Option(amount).map(BigDecimal(_)), BigDecimal(fee),
             asset.fold(ccy)(_.currency), external, counter, None, None, Instant.EPOCH.plusSeconds(seq))
  }

  private def replay(as: Activity*)(using c: Engine.Context = ctx()) = Engine.replay(acct, as.toList, c)

  test("buys include fees in cost; sells are FIFO with fees netted from proceeds") {
    val book = replay(
      act(ActivityType.Deposit, 1, amount = "10000"),
      act(ActivityType.Buy, 2, Some(aapl), "10", "100", fee = "5"),
      act(ActivityType.Buy, 3, Some(aapl), "10", "120"),
      act(ActivityType.Sell, 4, Some(aapl), "15", "150", fee = "3")
    ).toOption.get

    val pos = book.positions(aapl.id)
    assertEquals(pos.quantity, BigDecimal(5))
    assertEquals(pos.cost, BigDecimal(600))                     // half of the 1,200 lot remains
    assertEquals(book.cash(usd), BigDecimal(10000 - 1005 - 1200 + 2250 - 3))
    val realized = book.disposals.map(_.realized).sum
    assertEquals(realized, BigDecimal(2247) - BigDecimal(1005) - BigDecimal(600))
    assertEquals(book.disposals.map(_.quantity), Vector(BigDecimal(10), BigDecimal(5)))
    assertEquals(book.netContribution, BigDecimal(10000))
  }

  test("overselling is rejected with the symbol and the held quantity") {
    val res = replay(act(ActivityType.Buy, 2, Some(aapl), "3", "100"), act(ActivityType.Sell, 3, Some(aapl), "4", "100"))
    res match {
      case Left(PortfolioError.InsufficientQuantity(sym, held, req, _)) =>
        assertEquals(sym.value: String, "AAPL"); assertEquals(held, BigDecimal(3)); assertEquals(req, BigDecimal(4))
      case other => fail(s"unexpected $other")
    }
  }

  test("splits multiply quantity and keep cost") {
    val book = replay(act(ActivityType.Buy, 2, Some(aapl), "10", "100"), act(ActivityType.Split, 3, Some(aapl), amount = "4")).toOption.get
    assertEquals(book.positions(aapl.id).quantity, BigDecimal(40))
    assertEquals(book.positions(aapl.id).cost, BigDecimal(1000))
    assertEquals(book.positions(aapl.id).lastTradePrice, Some(BigDecimal(25)))
  }

  test("foreign lots carry base cost at the acquisition-date rate") {
    given Engine.Context = ctx(fx = List(FxRate(eur, usd, LocalDate.of(2026, 1, 1), BigDecimal("1.10")),
                                         FxRate(eur, usd, LocalDate.of(2026, 1, 10), BigDecimal("1.20"))))
    val book = replay(act(ActivityType.Buy, 5, Some(sap), "10", "100"), act(ActivityType.Sell, 15, Some(sap), "10", "100")).toOption.get
    val d = book.disposals.head
    assertEquals(d.costBase, BigDecimal(1100))
    assertEquals(d.proceedsBase, BigDecimal(1200))
    assertEquals(d.realized, BigDecimal(0))
    assertEquals(d.realizedBase, BigDecimal(100))            // a pure currency gain
  }

  test("a missing FX rate is an error, not a silent zero") {
    assertEquals(replay(act(ActivityType.Buy, 2, Some(sap), "1", "100")), Left(PortfolioError.MissingFxRate(eur, usd)))
  }

  test("income, fees and taxes move cash; only dividends and interest are income") {
    val book = replay(
      act(ActivityType.Deposit, 1, amount = "1000"),
      act(ActivityType.Dividend, 2, Some(aapl), amount = "30"),
      act(ActivityType.Interest, 3, amount = "5"),
      act(ActivityType.Fee, 4, amount = "2"),
      act(ActivityType.Tax, 5, amount = "4"),
      act(ActivityType.Credit, 6, amount = "10")
    ).toOption.get
    assertEquals(book.cash(usd), BigDecimal(1000 + 30 + 5 - 2 - 4 + 10))
    assertEquals(book.income.map(_.amount).sum, BigDecimal(35))
    assertEquals(book.netContribution, BigDecimal(1000))       // the credit is a rebate, not a contribution
  }

  test("transfers to a known account are internal; external ones are contributions") {
    val book = replay(
      act(ActivityType.TransferIn, 1, amount = "500", counter = Some(other)),
      act(ActivityType.TransferIn, 2, amount = "300", external = true),
      act(ActivityType.TransferIn, 3, Some(aapl), "2", "100", counter = Some(other))
    ).toOption.get
    assertEquals(book.netContribution, BigDecimal(300))
    assertEquals(book.flows.size, 3)
    assertEquals(book.positions(aapl.id).cost, BigDecimal(200))
    assert(Scope(Nil, Map.empty, Map.empty).counts(book.flows.head))
  }

  test("negative adjustments dispose at zero proceeds, booking the cost as a loss") {
    val book = replay(act(ActivityType.Buy, 2, Some(aapl), "5", "10"), act(ActivityType.Adjustment, 3, Some(aapl), "-5")).toOption.get
    assertEquals(book.positions(aapl.id).quantity, BigDecimal(0))
    assertEquals(book.disposals.map(_.realized).sum, BigDecimal(-50))
  }

  test("validation names what is missing") {
    assert(replay(act(ActivityType.Buy, 2, None, "1", "1")).isLeft)
    assert(replay(act(ActivityType.Deposit, 2)).isLeft)
    val wrongCcy = act(ActivityType.Buy, 2, Some(aapl), "1", "1").copy(currency = eur)
    assertEquals(Engine.validate(wrongCcy, Map(aapl.id -> aapl)), Left(PortfolioError.CurrencyMismatch(usd, eur)))
  }
}
