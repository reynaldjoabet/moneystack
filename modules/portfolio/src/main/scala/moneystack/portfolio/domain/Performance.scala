package moneystack.portfolio.domain

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

/**
 * Returns over a valuation series. Rates are fractions (0.05 = 5%).
 *
 *  - `twr`: time-weighted, linking daily returns with flows at end of day,
 *    so contributions don't count as performance.
 *  - `mwr`: money-weighted (XIRR, annualised) from the investor's cash flows.
 *  - Annualised figures need at least 30 days; volatility is the annualised
 *    standard deviation of daily returns.
 */
final case class Returns(
    from: LocalDate,
    to: LocalDate,
    startValue: BigDecimal,
    endValue: BigDecimal,
    netFlows: BigDecimal,
    gain: BigDecimal,
    twr: BigDecimal,
    twrAnnualized: Option[BigDecimal],
    mwr: Option[BigDecimal],
    volatility: Option[BigDecimal],
    maxDrawdown: BigDecimal
)

final case class ReturnPoint(date: LocalDate, value: BigDecimal, cumulativeTwr: BigDecimal)

object Performance {

  val DaysPerYear: Double        = 365.25
  val MinAnnualizationDays: Long = 30L

  /** Daily returns aren't measured off a base below one unit of currency. */
  private val MinBase = BigDecimal(1)

  private def scale(x: BigDecimal) = x.setScale(6, RoundingMode.HALF_EVEN)

  /** Daily time-weighted returns; the first day has no prior value and returns nothing. */
  def dailyReturns(series: List[DailyValue]): List[(LocalDate, BigDecimal)] =
    series.zip(series.drop(1)).map { case (prev, cur) =>
      val r = if (prev.total < MinBase) BigDecimal(0) else (cur.total - cur.flow - prev.total) / prev.total
      cur.date -> r
    }

  def curve(series: List[DailyValue]): List[ReturnPoint] =
    series.headOption.toList.map(h => ReturnPoint(h.date, h.total, BigDecimal(0))) ++
      dailyReturns(series).scanLeft(BigDecimal(1))((acc, r) => acc * (1 + r._2)).drop(1)
        .zip(series.drop(1)).map { case (index, dv) => ReturnPoint(dv.date, dv.total, scale(index - 1)) }

  def measure(series: List[DailyValue]): Option[Returns] =
    for {
      first <- series.headOption
      last  <- series.lastOption
    } yield {
      val daily  = dailyReturns(series).map(_._2)
      val index  = daily.scanLeft(BigDecimal(1))((acc, r) => acc * (1 + r))
      val twr    = index.last - 1
      val days   = ChronoUnit.DAYS.between(first.date, last.date)
      val flows  = series.drop(1).map(_.flow).sum
      Returns(
        from          = first.date,
        to            = last.date,
        startValue    = first.total,
        endValue      = last.total,
        netFlows      = flows,
        gain          = last.total - first.total - flows,
        twr           = scale(twr),
        twrAnnualized = annualize(twr, days).map(scale),
        mwr           = xirr(cashFlows(series)).map(scale),
        volatility    = volatility(daily).map(scale),
        maxDrawdown   = scale(maxDrawdown(index))
      )
    }

  def annualize(total: BigDecimal, days: Long): Option[BigDecimal] =
    Option.when(days >= MinAnnualizationDays && total > -1)(
      BigDecimal(math.pow((1 + total).toDouble, DaysPerYear / days.toDouble) - 1)
    )

  /** The investor's view: paying in is negative, the closing value is a final inflow. */
  def cashFlows(series: List[DailyValue]): List[(LocalDate, BigDecimal)] =
    series match {
      case Nil => Nil
      case first :: rest =>
        val opening = if (first.total != 0) List(first.date -> -first.total) else Nil
        opening ++ rest.filter(_.flow != 0).map(d => d.date -> -d.flow) ++ List(series.last.date -> series.last.total)
    }

  /** Annual internal rate of return: Newton's method, falling back to bisection. */
  def xirr(flows: List[(LocalDate, BigDecimal)]): Option[BigDecimal] = {
    val nonZero = flows.filter(_._2 != 0)
    if (nonZero.isEmpty || nonZero.forall(_._2 > 0) || nonZero.forall(_._2 < 0)) None
    else {
      val t0 = nonZero.map(_._1).min
      val ts = nonZero.map { case (d, v) => (ChronoUnit.DAYS.between(t0, d).toDouble / DaysPerYear, v.toDouble) }
      def npv(r: Double)  = ts.map { case (t, v) => v / math.pow(1 + r, t) }.sum
      def dnpv(r: Double) = ts.map { case (t, v) => -t * v / math.pow(1 + r, t + 1) }.sum

      @tailrec def newton(r: Double, i: Int): Option[Double] =
        if (i > 100 || r <= -0.999999 || r.isNaN || r.isInfinite) None
        else {
          val f = npv(r)
          if (math.abs(f) < 1e-7) Some(r)
          else {
            val d = dnpv(r)
            if (d == 0) None else newton(r - f / d, i + 1)
          }
        }

      @tailrec def bisect(lo: Double, hi: Double, i: Int): Option[Double] = {
        val mid = (lo + hi) / 2
        if (i > 200) Some(mid)
        else if (math.abs(npv(mid)) < 1e-7) Some(mid)
        else if (math.signum(npv(lo)) == math.signum(npv(mid))) bisect(mid, hi, i + 1)
        else bisect(lo, mid, i + 1)
      }

      newton(0.1, 0)
        .orElse(Option.when(math.signum(npv(-0.9999)) != math.signum(npv(10.0)))(()).flatMap(_ => bisect(-0.9999, 10.0, 0)))
        .map(BigDecimal(_))
    }
  }

  def volatility(daily: List[BigDecimal]): Option[BigDecimal] =
    Option.when(daily.size >= 2) {
      val xs   = daily.map(_.toDouble)
      val mean = xs.sum / xs.size
      val varS = xs.map(x => (x - mean) * (x - mean)).sum / (xs.size - 1)
      BigDecimal(math.sqrt(varS) * math.sqrt(DaysPerYear))
    }

  /** Largest peak-to-trough fall of the growth index, as a non-positive fraction. */
  def maxDrawdown(index: List[BigDecimal]): BigDecimal =
    index.foldLeft((BigDecimal(0), BigDecimal(0))) { case ((peak, worst), v) =>
      val p = peak.max(v)
      (p, if (p == 0) worst else worst.min(v / p - 1))
    }._2
}
