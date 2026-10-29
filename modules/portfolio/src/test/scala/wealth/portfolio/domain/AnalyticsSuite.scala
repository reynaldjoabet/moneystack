package wealth.portfolio.domain

import wealth.domain.*

import java.time.LocalDate
import java.util.UUID

class AnalyticsSuite extends munit.FunSuite {

  private def holding(kind: AssetKind, ccy: CurrencyCode, value: Int) = {
    val id = AssetId.assume(UUID.randomUUID())
    HoldingView(id, Symbol.applyUnsafe("X"), Title.applyUnsafe("X"), kind, ccy, BigDecimal(1), BigDecimal(1), BigDecimal(value), true,
                BigDecimal(value), BigDecimal(value), BigDecimal(value), BigDecimal(0), None, BigDecimal(0), Nil)
  }

  private val owner = UserId.assume(UUID.randomUUID())
  private def target(cat: String, bps: Int) =
    AllocationTarget(TargetId.assume(UUID.randomUUID()), owner, Taxonomy.applyUnsafe("asset_class"), Title.applyUnsafe(cat), Bps.assume(bps))

  test("allocation by asset kind includes cash") {
    val s = Analytics.allocation(List(holding(AssetKind.Etf, CurrencyCode.USD, 600), holding(AssetKind.Bond, CurrencyCode.EUR, 300)),
                                 BigDecimal(100), Analytics.AssetKindTaxonomy, Nil)
    assertEquals(s.map(x => x.category -> x.pct.toInt), List("etf" -> 60, "bond" -> 30, "Cash" -> 10))
  }

  test("weighted classifications split a holding; the remainder is unclassified") {
    val etf = holding(AssetKind.Etf, CurrencyCode.USD, 1000)
    val cs  = List(Classification(etf.assetId, Taxonomy.Region, Title.applyUnsafe("US"), Bps.assume(6000)),
                   Classification(etf.assetId, Taxonomy.Region, Title.applyUnsafe("Europe"), Bps.assume(3000)))
    val s = Analytics.allocation(List(etf), BigDecimal(0), "region", cs)
    assertEquals(s.map(x => x.category -> x.value.toInt).toSet, Set("US" -> 600, "Europe" -> 300, "Unclassified" -> 100))
  }

  test("targets must be distinct and total 100%") {
    assert(Analytics.validateTargets(List("a" -> 5000, "b" -> 5000)).isRight)
    assert(Analytics.validateTargets(List("a" -> 5000, "b" -> 4000)).isLeft)
    assert(Analytics.validateTargets(List("a" -> 5000, "a" -> 5000)).isLeft)
  }

  test("buy-only rebalancing puts new cash where the shortfall is; full rebalancing lands on target") {
    val slices  = List(Slice("stocks", BigDecimal(800), BigDecimal(80)), Slice("bonds", BigDecimal(200), BigDecimal(20)))
    val targets = List(target("stocks", 6000), target("bonds", 4000))
    val buy = Analytics.rebalance(slices, targets, BigDecimal(500), RebalanceMode.BuyOnly).map(r => r.category -> r.trade.toInt).toMap
    assertEquals(buy, Map("bonds" -> 400, "stocks" -> 100))   // targets 600 / 900 of 1,500
    val full = Analytics.rebalance(slices, targets, BigDecimal(0), RebalanceMode.Full).map(r => r.category -> r.trade.toInt).toMap
    assertEquals(full, Map("bonds" -> 200, "stocks" -> -200))
    val drift = Analytics.drift(slices, targets).map(r => r.category -> r.driftPct.toInt).toMap
    assertEquals(drift, Map("bonds" -> -20, "stocks" -> 20))
  }

  test("contribution limits count deposits and external transfers in the window") {
    val acct = InvestmentAccountId.assume(UUID.randomUUID())
    def flow(m: Int, amount: Int, transfer: Boolean = false, counter: Option[InvestmentAccountId] = None) =
      Flow(LocalDate.of(2026, m, 1), BigDecimal(amount), transfer, counter)
    val book  = Book.empty(acct).copy(flows = Vector(flow(1, 3000), flow(2, -500), flow(3, 1000, transfer = true),
                                                     flow(4, 2000, transfer = true, counter = Some(InvestmentAccountId.assume(UUID.randomUUID())))))
    val limit = ContributionLimit(LimitId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("ISA"), 2026, BigDecimal(20000),
                                  CurrencyCode.USD, List(acct), None, None)
    val p = Analytics.limitProgress(limit, List(book), FxTable.empty, CurrencyCode.USD).toOption.get
    assertEquals(p.contributed, BigDecimal("4000.00"))
    assertEquals(p.remaining, BigDecimal("16000.00"))
  }
}
