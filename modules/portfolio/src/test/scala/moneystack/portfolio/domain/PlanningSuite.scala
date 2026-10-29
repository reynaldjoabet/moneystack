package moneystack.portfolio.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate, YearMonth}
import java.util.UUID

class PlanningSuite extends munit.FunSuite {

  private val asOf = LocalDate.of(2026, 1, 15)

  test("save-up at zero return needs exactly the remaining amount per month") {
    val in = SaveUpInput(BigDecimal(0), BigDecimal(1200), Some(LocalDate.of(2027, 1, 15)), BigDecimal(100), BigDecimal(0))
    val o  = SaveUp.overview(in, asOf)
    assertEquals(o.health, "on_track")
    assertEquals(o.projectedAtTargetDate, Some(BigDecimal("1200.00")))
    assertEquals(o.requiredMonthlyContribution, Some(BigDecimal("100.00")))
    assertEquals(o.projectedCompletion.map(YearMonth.from), Some(YearMonth.of(2026, 12)))
    assertEquals(o.trajectory.size, 12)
  }

  test("save-up health: at risk within 90% of target, off track below") {
    def health(monthly: Int) =
      SaveUp.overview(SaveUpInput(BigDecimal(0), BigDecimal(1000), Some(LocalDate.of(2026, 11, 15)), BigDecimal(monthly), BigDecimal(0)), asOf).health
    assertEquals(health(100), "on_track")
    assertEquals(health(92), "at_risk")
    assertEquals(health(50), "off_track")
  }

  test("save-up scenarios bracket the nominal path") {
    val o = SaveUp.overview(SaveUpInput(BigDecimal(10000), BigDecimal(50000), Some(LocalDate.of(2031, 1, 1)), BigDecimal(300), BigDecimal("0.06")), asOf)
    val last = o.trajectory.last
    assert(last.pessimistic < last.nominal && last.nominal < last.optimistic)
    assert(o.projectedAtTargetDate.get > BigDecimal(10000 + 300 * 59))
  }

  private val fire = FireInput(BigDecimal(500_000), BigDecimal(40_000), BigDecimal(100_000), BigDecimal("0.025"), BigDecimal(0),
                               BigDecimal("0.025"), BigDecimal("0.04"), 40, None, 90, 200, 7L)

  test("FIRE: the FI number is expenses over the withdrawal rate; zero real return makes it arithmetic") {
    val r = Fire.run(fire.copy(horizonAge = 69))
    assertEquals(r.fireNumber, BigDecimal("1000000.00"))
    assertEquals(r.yearsToFi, Some(5))
    assertEquals(r.fiAge, Some(45))
    assertEquals(r.retireAt, 45)
    // zero volatility: every simulation matches the deterministic path; 24 years of 40k from 1M leaves 40k
    assertEquals(r.successRate, BigDecimal(1).setScale(4))
    assert(r.projection.find(_.age == 45).exists(_.withdrawal == BigDecimal("40000.00")))
  }

  test("FIRE: volatility spreads outcomes and the seed makes them repeatable") {
    val noisy = fire.copy(expectedReturn = BigDecimal("0.07"), returnVolatility = BigDecimal("0.18"))
    val a = Fire.run(noisy); val b = Fire.run(noisy)
    assertEquals(a.successRate, b.successRate)
    val last = a.percentiles.last
    assert(last.p10 <= last.p50 && last.p50 <= last.p90)
    assert(Fire.validate(fire.copy(withdrawalRate = BigDecimal(0))).isLeft)
  }

  private def loan(rate: String, months: Int, extra: Int = 0) =
    Liability(LiabilityId.assume(UUID.randomUUID()), UserId.assume(UUID.randomUUID()), LiabilityKind.Mortgage, Title.applyUnsafe("Home"),
              CurrencyCode.USD, BigDecimal(100_000), BigDecimal(rate), months, LocalDate.of(2026, 1, 1), BigDecimal(extra), None, true, Instant.EPOCH)

  test("annuity payment and a schedule that pays off on time") {
    val l = loan("0.06", 360)
    assertEquals(Amortization.monthlyPayment(l.principal, l.annualRate, 360), BigDecimal("599.55"))
    val rows = Amortization.forLiability(l)
    assert(rows.size <= 361 && rows.size >= 359, rows.size.toString)
    assertEquals(rows.last.balance, BigDecimal("0.00"))
    assertEquals(Amortization.monthlyPayment(BigDecimal(1200), BigDecimal(0), 12), BigDecimal("100.00"))
  }

  test("extra payments shorten the loan; statements re-anchor the balance") {
    val l = loan("0.06", 360, extra = 400)
    assert(Amortization.forLiability(l).size < 200)
    val stated = List(LiabilityStatement(l.id, LocalDate.of(2027, 1, 1), BigDecimal(50_000)))
    assertEquals(Amortization.balanceOn(l, stated, LocalDate.of(2027, 1, 1)), BigDecimal("50000.00"))
    assert(Amortization.balanceOn(l, stated, LocalDate.of(2027, 3, 1)) < BigDecimal(50_000))
    assertEquals(Amortization.balanceOn(l, Nil, LocalDate.of(2025, 12, 1)), BigDecimal(0))
  }

  test("vehicle depreciation follows the class curve, scaled by pace") {
    val bought = LocalDate.of(2024, 1, 1)
    assertEquals(Depreciation.value(BigDecimal(20_000), bought, VehicleClass.Sedan, DepreciationPace.Average, bought), BigDecimal(20_000))
    val oneYear = Depreciation.value(BigDecimal(20_000), bought, VehicleClass.Sedan, DepreciationPace.Average, bought.plusDays(366))
    assert(math.abs(oneYear.toDouble - 16_000) < 20, oneYear.toString)
    val slow = Depreciation.value(BigDecimal(20_000), bought, VehicleClass.Sedan, DepreciationPace.Slow, bought.plusDays(366))
    assert(slow > oneYear)
  }
}
