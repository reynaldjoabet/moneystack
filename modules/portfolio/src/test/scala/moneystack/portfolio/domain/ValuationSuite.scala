package moneystack.portfolio.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

class ValuationSuite extends munit.FunSuite {

  private val usd = CurrencyCode.USD
  private val d0  = LocalDate.of(2026, 1, 1)

  private def day(i: Int) = d0.plusDays(i.toLong)

  private val aapl =
    Asset(AssetId.assume(UUID.randomUUID()), Symbol.applyUnsafe("AAPL"), Title.applyUnsafe("AAPL"), AssetKind.Equity, usd, Instant.EPOCH)

  private def account(tracking: Tracking) =
    InvestmentAccount(
      InvestmentAccountId.assume(UUID.randomUUID()),
      UserId.assume(UUID.randomUUID()),
      Title.applyUnsafe("Account"),
      AccountKind.Brokerage,
      tracking,
      usd,
      None,
      true,
      Instant.EPOCH
    )

  private val broker  = account(Tracking.Activities)
  private val pension = account(Tracking.Balances)

  private def activity(kind: ActivityType, i: Int, asset: Option[Asset], qty: String, price: String, amount: String) =
    Activity(
      ActivityId.assume(UUID.randomUUID()),
      broker.id,
      asset.map(_.id),
      kind,
      day(i),
      Option(qty).map(BigDecimal(_)),
      Option(price).map(BigDecimal(_)),
      Option(amount).map(BigDecimal(_)),
      BigDecimal(0),
      usd,
      true,
      None,
      None,
      None,
      Instant.EPOCH.plusSeconds(i.toLong)
    )

  private val scope = Scope(
    List(broker, pension),
    Map(
      broker.id -> List(
        activity(ActivityType.Deposit, 0, None, null, null, "10000"),
        activity(ActivityType.Buy, 1, Some(aapl), "10", "100", null),
        activity(ActivityType.Deposit, 5, None, null, null, "500"),
        activity(ActivityType.Sell, 8, Some(aapl), "4", "130", null),
        activity(ActivityType.Withdrawal, 12, None, null, null, "250"),
        activity(ActivityType.Buy, 25, Some(aapl), "2", "150", null)
      )
    ),
    Map(pension.id -> List(
      BalanceSnapshot(pension.id, day(3), BigDecimal(1000), BigDecimal(900)),
      BalanceSnapshot(pension.id, day(20), BigDecimal(1200), BigDecimal(1000))
    ))
  )

  private val ctx = Engine.Context(
    Map(aapl.id -> aapl),
    PriceTable((0 to 40 by 3).toList.map(i => Quote(aapl.id, day(i), BigDecimal(100 + i)))),
    FxTable.empty,
    usd
  )

  private val whole = Valuation.series(scope, ctx, day(0), day(30)).toOption.get

  test("a series that starts late equals the same days of a full one") {
    (1 to 30).foreach { start =>
      assertEquals(Valuation.series(scope, ctx, day(start), day(30)), Right(whole.drop(start)), s"from day $start")
    }
  }

  test("a single day carries what was contributed before it") {
    val only = Valuation.series(scope, ctx, day(30), day(30)).toOption.get
    assertEquals(only, List(whole.last))
    assert(only.head.netContribution > 0)
  }
}
