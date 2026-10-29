package moneystack.portfolio.domain

import java.time.LocalDate

class PerformanceSuite extends munit.FunSuite {

  private val d0 = LocalDate.of(2026, 1, 1)

  private def day(i: Int, total: Int, flow: Int = 0) =
    DailyValue(d0.plusDays(i.toLong), BigDecimal(total), BigDecimal(0), BigDecimal(total), BigDecimal(0), BigDecimal(0), BigDecimal(flow))

  private def close(a: BigDecimal, b: Double, tol: Double = 1e-4) = assert(math.abs(a.toDouble - b) < tol, s"$a vs $b")

  test("time-weighted return ignores contributions") {
    // +10%, then a 100 deposit, then +10% on the new base
    val series = List(day(0, 100), day(1, 110), day(2, 210, flow = 100), day(3, 231))
    val r      = Performance.measure(series).get
    close(r.twr, 0.21)
    assertEquals(r.netFlows, BigDecimal(100))
    assertEquals(r.gain, BigDecimal(31))
    assertEquals(r.twrAnnualized, None) // under 30 days
  }

  test("a deposit into an empty account is not a return") {
    val series = List(day(0, 0), day(1, 1000, flow = 1000), day(2, 1050))
    close(Performance.measure(series).get.twr, 0.05)
  }

  test("xirr of a 10% year") {
    val r = Performance.xirr(List(d0 -> BigDecimal(-1000), d0.plusDays(365) -> BigDecimal(1100))).get
    close(r, 0.10, 2e-3)
    assertEquals(Performance.xirr(List(d0 -> BigDecimal(5))), None)
  }

  test("money-weighted return weights the larger, later capital") {
    // small gain on 100, then a big loss after adding 1000: MWR is far below TWR
    val series = (0 to 60).toList.map { i =>
      if (i < 30) day(i, 100 + i) else if (i == 30) day(i, 1130, flow = 1000) else day(i, 1130 - (i - 30) * 5)
    }
    val r = Performance.measure(series).get
    assert(r.mwr.get < r.twrAnnualized.get, s"mwr ${r.mwr} twr ${r.twrAnnualized}")
  }

  test("max drawdown and annualisation") {
    assertEquals(Performance.maxDrawdown(List(1, 1.2, 0.9, 1.1).map(BigDecimal(_))), BigDecimal("-0.25"))
    close(Performance.annualize(BigDecimal("0.21"), 730).get, 0.1, 1e-3)
    assertEquals(Performance.annualize(BigDecimal("0.01"), 10), None)
  }

  test("investor cash flows: opening value and deposits out, closing value in") {
    val flows = Performance.cashFlows(List(day(0, 100), day(1, 210, flow = 100), day(2, 220)))
    assertEquals(flows.map(_._2), List(BigDecimal(-100), BigDecimal(-100), BigDecimal(220)))
  }
}
