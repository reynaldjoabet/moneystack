package wealth.budget.domain

import java.time.LocalDate
import java.time.temporal.ChronoUnit

/** An inclusive date range. */
final case class Period(from: LocalDate, to: LocalDate) {
  def contains(d: LocalDate): Boolean = !d.isBefore(from) && !d.isAfter(to)
  def days: Long                      = ChronoUnit.DAYS.between(from, to) + 1
}

object Periods {

  private def unit(r: Recurrence): ChronoUnit = r match {
    case Recurrence.Daily   => ChronoUnit.DAYS
    case Recurrence.Weekly  => ChronoUnit.WEEKS
    case Recurrence.Monthly => ChronoUnit.MONTHS
    case Recurrence.Yearly  => ChronoUnit.YEARS
    case Recurrence.Custom  => ChronoUnit.FOREVER
  }

  /** Start of the k-th period (from the anchor, not cumulative, so month-end anchors don't drift). */
  private def startOf(b: Budget, k: Long): LocalDate = b.startDate.plus(k * b.periodLength, unit(b.recurrence))

  /** The budget period containing `date`, if the budget is running then. */
  def periodOf(b: Budget, date: LocalDate): Option[Period] =
    b.recurrence match {
      case Recurrence.Custom =>
        b.endDate.map(Period(b.startDate, _)).filter(_.contains(date))
      case r =>
        if (date.isBefore(b.startDate)) None
        else {
          // `between` truncates on calendar units (Jan 31 -> Feb 28 is "0 months"), so estimate, then adjust.
          var k = unit(r).between(b.startDate, date) / b.periodLength
          while (!startOf(b, k + 1).isAfter(date)) k += 1
          while (k > 0 && startOf(b, k).isAfter(date)) k -= 1
          val p = Period(startOf(b, k), startOf(b, k + 1).minusDays(1))
          Option.when(b.endDate.forall(!p.from.isAfter(_)))(p.copy(to = b.endDate.fold(p.to)(e => if (e.isBefore(p.to)) e else p.to)))
        }
    }

  /** The period before `p`, for rollover and comparisons. */
  def previous(b: Budget, p: Period): Option[Period] = periodOf(b, p.from.minusDays(1))

  // ---- schedules ----

  private def stepUnit(f: Frequency): ChronoUnit = f match {
    case Frequency.Daily   => ChronoUnit.DAYS
    case Frequency.Weekly  => ChronoUnit.WEEKS
    case Frequency.Monthly => ChronoUnit.MONTHS
    case Frequency.Yearly  => ChronoUnit.YEARS
  }

  /** Due date of occurrence `n`, if the schedule still runs then. */
  def due(s: Schedule, n: Int): Option[LocalDate] =
    Option.when(n >= 0 && s.maxOccurrences.forall(n < _)) {
      s.startDate.plus(n.toLong * s.interval, stepUnit(s.frequency))
    }.filter(d => s.endDate.forall(!d.isAfter(_)))

  def nextDue(s: Schedule): Option[LocalDate] = if (s.active) due(s, s.nextIndex) else None

  /** Unpaid occurrences due in `[from, to]`, with their indices. */
  def occurrences(s: Schedule, from: LocalDate, to: LocalDate): List[(Int, LocalDate)] =
    if (!s.active) Nil
    else
      Iterator.from(s.nextIndex).map(n => n -> due(s, n)).takeWhile { case (_, d) => d.exists(!_.isAfter(to)) }
        .collect { case (n, Some(d)) if !d.isBefore(from) => n -> d }.toList

  /** Monthly-equivalent cost, for comparing subscriptions billed at different frequencies. */
  def monthlyCost(s: Schedule): BigDecimal = {
    val perUnit = s.frequency match {
      case Frequency.Daily   => BigDecimal(365) / 12
      case Frequency.Weekly  => BigDecimal(52) / 12
      case Frequency.Monthly => BigDecimal(1)
      case Frequency.Yearly  => BigDecimal(1) / 12
    }
    (BigDecimal(s.amountMinor) * perUnit / s.interval).setScale(0, BigDecimal.RoundingMode.HALF_EVEN)
  }
}
