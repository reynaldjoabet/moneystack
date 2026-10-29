package moneystack.budget.domain

import Fixtures.*

class PeriodsSuite extends munit.FunSuite {

  test("monthly budgets anchored on the 31st clamp to short months without drifting") {
    val b = budget(Recurrence.Monthly, d(1, 31))
    assertEquals(Periods.periodOf(b, d(2, 15)), Some(Period(d(1, 31), d(2, 27))))
    assertEquals(Periods.periodOf(b, d(2, 28)), Some(Period(d(2, 28), d(3, 30))))
    assertEquals(Periods.periodOf(b, d(3, 31)), Some(Period(d(3, 31), d(4, 29))))   // back on the 31st
    assertEquals(Periods.periodOf(b, d(1, 30)), None)
  }

  test("weekly, fortnightly, yearly and custom periods") {
    assertEquals(Periods.periodOf(budget(Recurrence.Weekly, d(1, 5)), d(1, 14)), Some(Period(d(1, 12), d(1, 18))))
    assertEquals(Periods.periodOf(budget(Recurrence.Weekly, d(1, 5), length = 2), d(1, 20)), Some(Period(d(1, 19), d(2, 1))))
    assertEquals(Periods.periodOf(budget(Recurrence.Yearly, d(4, 6)), d(1, 1, 2027)), Some(Period(d(4, 6), d(4, 5, 2027))))
    val custom = budget(Recurrence.Custom, d(6, 1), end = Some(d(6, 20)))
    assertEquals(Periods.periodOf(custom, d(6, 10)), Some(Period(d(6, 1), d(6, 20))))
    assertEquals(Periods.periodOf(custom, d(6, 21)), None)
  }

  test("an end date truncates the last period and stops the budget") {
    val b = budget(Recurrence.Monthly, d(1, 1), end = Some(d(3, 15)))
    assertEquals(Periods.periodOf(b, d(3, 10)), Some(Period(d(3, 1), d(3, 15))))
    assertEquals(Periods.periodOf(b, d(4, 1)), None)
    assertEquals(Periods.previous(b, Period(d(3, 1), d(3, 15))), Some(Period(d(2, 1), d(2, 28))))
  }

  test("schedule due dates keep the anchor day and respect limits") {
    val s = schedule(Frequency.Monthly, d(1, 31))
    assertEquals((0 to 3).toList.flatMap(Periods.due(s, _)), List(d(1, 31), d(2, 28), d(3, 31), d(4, 30)))
    val instalments = schedule(Frequency.Monthly, d(1, 1), max = Some(3))
    assertEquals(Periods.due(instalments, 3), None)
    val ending = schedule(Frequency.Weekly, d(1, 1), interval = 2, end = Some(d(1, 20)))
    assertEquals(Periods.occurrences(ending, d(1, 1), d(12, 31)).map(_._2), List(d(1, 1), d(1, 15)))
    assertEquals(Periods.nextDue(instalments.copy(nextIndex = 3)), None)
    assertEquals(Periods.nextDue(s.copy(active = false)), None)
  }

  test("monthly-equivalent cost") {
    assertEquals(Periods.monthlyCost(schedule(Frequency.Monthly, d(1, 1))), BigDecimal(1599))
    assertEquals(Periods.monthlyCost(schedule(Frequency.Yearly, d(1, 1)).copy(amountMinor = 12000)), BigDecimal(1000))
    assertEquals(Periods.monthlyCost(schedule(Frequency.Weekly, d(1, 1)).copy(amountMinor = 1200)), BigDecimal(5200))
    assertEquals(Periods.monthlyCost(schedule(Frequency.Monthly, d(1, 1), interval = 3).copy(amountMinor = 3000)), BigDecimal(1000))
  }
}
