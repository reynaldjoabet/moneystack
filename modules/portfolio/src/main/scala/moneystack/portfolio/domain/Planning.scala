package moneystack.portfolio.domain

import java.time.{LocalDate, YearMonth}
import java.time.temporal.ChronoUnit
import scala.math.BigDecimal.RoundingMode

// ---------- Save-up goals ----------

final case class SaveUpInput(
    currentValue: BigDecimal,
    targetAmount: BigDecimal,
    targetDate: Option[LocalDate],
    monthlyContribution: BigDecimal,
    expectedAnnualReturn: BigDecimal
)

final case class SaveUpPoint(month: YearMonth, nominal: BigDecimal, optimistic: BigDecimal, pessimistic: BigDecimal, target: BigDecimal)

/** `health` is on_track, at_risk (within 90% of target), off_track, or not_applicable. */
final case class SaveUpOverview(
    progress: BigDecimal,
    health: String,
    projectedAtTargetDate: Option[BigDecimal],
    requiredMonthlyContribution: Option[BigDecimal],
    projectedCompletion: Option[LocalDate],
    trajectory: List[SaveUpPoint]
)

/**
 * Daily compounding over actual calendar days, contributions at the end of
 * each month; optimistic and pessimistic scenarios are the expected return
 * plus and minus two points (pessimistic never below zero).
 */
object SaveUp {

  val MaxMonths = 1200

  private def money(x: BigDecimal) = x.setScale(2, RoundingMode.HALF_EVEN)

  /** End-of-month dates from `asOf`, `months` of them. */
  private def monthEnds(asOf: LocalDate, months: Int): List[LocalDate] =
    (1 to months).toList.map(i => YearMonth.from(asOf).plusMonths(i.toLong - 1).atEndOfMonth).map(d => if (d.isBefore(asOf)) asOf else d)

  /** Balance at each month end. */
  def path(current: BigDecimal, monthly: BigDecimal, annualReturn: BigDecimal, asOf: LocalDate, months: Int): List[BigDecimal] = {
    val daily = annualReturn.toDouble / 365.0
    monthEnds(asOf, months).scanLeft((asOf, current.toDouble)) { case ((prev, bal), end) =>
      val days = ChronoUnit.DAYS.between(prev, end).toDouble
      (end, bal * math.pow(1 + daily, days) + monthly.toDouble)
    }.drop(1).map(p => BigDecimal(p._2))
  }

  def monthsBetween(from: LocalDate, to: LocalDate): Int =
    ChronoUnit.MONTHS.between(YearMonth.from(from), YearMonth.from(to)).toInt.max(0)

  def overview(in: SaveUpInput, asOf: LocalDate): SaveUpOverview = {
    val progress = if (in.targetAmount <= 0) BigDecimal(0) else (in.currentValue / in.targetAmount).min(1).setScale(4, RoundingMode.HALF_EVEN)
    in.targetDate match {
      case Some(td) =>
        val months    = monthsBetween(asOf, td).min(MaxMonths)
        val projected = if (months == 0) in.currentValue else path(in.currentValue, in.monthlyContribution, in.expectedAnnualReturn, asOf, months).last
        val health =
          if (in.targetAmount <= 0) "not_applicable"
          else if (projected >= in.targetAmount) "on_track"
          else if (projected >= in.targetAmount * BigDecimal("0.9")) "at_risk"
          else "off_track"
        SaveUpOverview(progress, health, Some(money(projected)), Some(money(requiredMonthly(in, asOf, months))),
                       completion(in, asOf), trajectory(in, asOf, months))
      case None =>
        val done = completion(in, asOf)
        val months = done.fold(MaxMonths.min(120))(d => monthsBetween(asOf, d).max(1))
        SaveUpOverview(progress, if (in.targetAmount <= 0) "not_applicable" else if (done.isDefined) "on_track" else "off_track",
                       None, None, done, trajectory(in, asOf, months))
    }
  }

  /** The contribution that reaches the target by the deadline: FV is linear in the contribution. */
  def requiredMonthly(in: SaveUpInput, asOf: LocalDate, months: Int): BigDecimal =
    if (months <= 0) (in.targetAmount - in.currentValue).max(0)
    else {
      val withNothing = path(in.currentValue, BigDecimal(0), in.expectedAnnualReturn, asOf, months).last
      val perUnit     = path(BigDecimal(0), BigDecimal(1), in.expectedAnnualReturn, asOf, months).last
      ((in.targetAmount - withNothing) / perUnit).max(0)
    }

  /** The month end at which the nominal path first reaches the target, within the horizon. */
  def completion(in: SaveUpInput, asOf: LocalDate): Option[LocalDate] =
    if (in.currentValue >= in.targetAmount) Some(asOf)
    else
      path(in.currentValue, in.monthlyContribution, in.expectedAnnualReturn, asOf, MaxMonths)
        .zip(monthEnds(asOf, MaxMonths))
        .collectFirst { case (bal, d) if bal >= in.targetAmount => d }

  def trajectory(in: SaveUpInput, asOf: LocalDate, months: Int): List[SaveUpPoint] =
    if (months <= 0) Nil
    else {
      val two = BigDecimal("0.02")
      val nominal = path(in.currentValue, in.monthlyContribution, in.expectedAnnualReturn, asOf, months)
      val up      = path(in.currentValue, in.monthlyContribution, in.expectedAnnualReturn + two, asOf, months)
      val down    = path(in.currentValue, in.monthlyContribution, (in.expectedAnnualReturn - two).max(0), asOf, months)
      monthEnds(asOf, months).zip(nominal.zip(up).zip(down)).map { case (d, ((n, o), p)) =>
        SaveUpPoint(YearMonth.from(d), money(n), money(o), money(p), money(in.targetAmount))
      }
    }
}

// ---------- Financial independence ----------

/**
 * Inputs in today's money. Returns are nominal and deflated by `inflation`;
 * the FI number is annual expenses over the safe withdrawal rate.
 */
final case class FireInput(
    currentPortfolio: BigDecimal,
    annualExpenses: BigDecimal,
    annualSavings: BigDecimal,
    expectedReturn: BigDecimal,
    returnVolatility: BigDecimal,
    inflation: BigDecimal,
    withdrawalRate: BigDecimal,
    currentAge: Int,
    retirementAge: Option[Int],
    horizonAge: Int,
    simulations: Int,
    seed: Long
)

final case class FireYear(age: Int, start: BigDecimal, contribution: BigDecimal, growth: BigDecimal, withdrawal: BigDecimal, end: BigDecimal)

final case class FirePercentiles(age: Int, p10: BigDecimal, p50: BigDecimal, p90: BigDecimal)

/** `successRate`: share of simulations whose money lasts to the horizon when retiring at `retireAt`. */
final case class FireResult(
    fireNumber: BigDecimal,
    realReturn: BigDecimal,
    fiAge: Option[Int],
    yearsToFi: Option[Int],
    retireAt: Int,
    projection: List[FireYear],
    successRate: BigDecimal,
    reachedFiRate: BigDecimal,
    percentiles: List[FirePercentiles]
)

object Fire {

  private def money(x: Double) = BigDecimal(x).setScale(2, RoundingMode.HALF_EVEN)

  def realReturn(nominal: BigDecimal, inflation: BigDecimal): BigDecimal = (1 + nominal) / (1 + inflation) - 1

  def validate(in: FireInput): Either[PortfolioError, Unit] = {
    def need(ok: Boolean, msg: String) = Either.cond(ok, (), PortfolioError.InvalidInput(msg))
    for {
      _ <- need(in.withdrawalRate > 0 && in.withdrawalRate < 1, "withdrawal rate must be between 0 and 1")
      _ <- need(in.annualExpenses > 0, "annual expenses must be positive")
      _ <- need(in.currentAge >= 0 && in.horizonAge > in.currentAge && in.horizonAge <= 120, "horizon age must be after the current age (max 120)")
      _ <- need(in.retirementAge.forall(a => a >= in.currentAge && a <= in.horizonAge), "retirement age must lie between the current and horizon ages")
      _ <- need(in.simulations >= 0 && in.simulations <= 20_000, "simulations must be between 0 and 20,000")
      _ <- need(in.returnVolatility >= 0, "volatility must not be negative")
    } yield ()
  }

  def run(in: FireInput): FireResult = {
    val fireNumber = in.annualExpenses / in.withdrawalRate
    val real       = realReturn(in.expectedReturn, in.inflation).toDouble
    val years      = in.horizonAge - in.currentAge

    // Deterministic accumulation: the first age whose opening balance covers the FI number.
    val balances = (0 to years).scanLeft(in.currentPortfolio.toDouble)((b, _) => b * (1 + real) + in.annualSavings.toDouble).toList
    val fiAge    = balances.zipWithIndex.collectFirst { case (b, i) if b >= fireNumber.toDouble => in.currentAge + i }
    val retireAt = in.retirementAge.orElse(fiAge).getOrElse(in.horizonAge)

    val projection = simulatePath(in, retireAt, List.fill(years)(real)).years

    val rng  = new scala.util.Random(in.seed)
    val sims = (1 to in.simulations).toList.map { _ =>
      simulatePath(in, retireAt, List.fill(years)(real + in.returnVolatility.toDouble * rng.nextGaussian()))
    }
    val success   = if (sims.isEmpty) BigDecimal(0) else BigDecimal(sims.count(_.lasted).toDouble / sims.size)
    val reachedFi = if (sims.isEmpty) BigDecimal(0) else BigDecimal(sims.count(_.reachedFi(fireNumber.toDouble, retireAt - in.currentAge)).toDouble / sims.size)
    val pct = (0 until years).toList.map { i =>
      val ends = sims.map(_.ends(i)).sorted
      def q(p: Double) = if (ends.isEmpty) BigDecimal(0) else money(ends(((ends.size - 1) * p).round.toInt))
      FirePercentiles(in.currentAge + i + 1, q(0.1), q(0.5), q(0.9))
    }

    FireResult(fireNumber.setScale(2, RoundingMode.HALF_EVEN), realReturn(in.expectedReturn, in.inflation).setScale(6, RoundingMode.HALF_EVEN),
               fiAge, fiAge.map(_ - in.currentAge), retireAt, projection,
               success.setScale(4, RoundingMode.HALF_EVEN), reachedFi.setScale(4, RoundingMode.HALF_EVEN), pct)
  }

  private final case class Path(years: List[FireYear], ends: Vector[Double], lasted: Boolean) {
    def reachedFi(fireNumber: Double, yearsToRetire: Int): Boolean =
      yearsToRetire <= 0 || ends.lift(yearsToRetire - 1).exists(_ >= fireNumber)
  }

  /** Saves until `retireAt`, then withdraws expenses each year; money must not run out before the horizon. */
  private def simulatePath(in: FireInput, retireAt: Int, returns: List[Double]): Path = {
    val (rows, _, lasted) = returns.zipWithIndex.foldLeft((Vector.empty[FireYear], in.currentPortfolio.toDouble, true)) {
      case ((acc, bal, ok), (r, i)) =>
        val age        = in.currentAge + i
        val retired    = age >= retireAt
        val contrib    = if (retired) 0.0 else in.annualSavings.toDouble
        val withdrawal = if (retired) in.annualExpenses.toDouble else 0.0
        val growth     = bal * r
        val end        = (bal + growth + contrib - withdrawal).max(0.0)
        (acc :+ FireYear(age, money(bal), money(contrib), money(growth), money(withdrawal), money(end)), end, ok && (!retired || bal + growth + contrib - withdrawal > 0))
    }
    Path(rows.toList, rows.map(_.end.toDouble), lasted)
  }
}

// ---------- Loans ----------

final case class AmortizationRow(number: Int, date: LocalDate, payment: BigDecimal, interest: BigDecimal, principal: BigDecimal, balance: BigDecimal)

/** Fixed-payment (annuity) amortisation with monthly payments one month apart from the anchor. */
object Amortization {

  private def money(x: BigDecimal) = x.setScale(2, RoundingMode.HALF_EVEN)

  def monthlyPayment(principal: BigDecimal, annualRate: BigDecimal, months: Int): BigDecimal =
    if (months <= 0) principal
    else if (annualRate == 0) money(principal / months)
    else {
      val r = (annualRate / 12).toDouble
      money(principal * BigDecimal(r / (1 - math.pow(1 + r, -months.toDouble))))
    }

  /** Remaining payments from a balance known on `anchor`, with the original contractual payment. */
  def schedule(balance: BigDecimal, annualRate: BigDecimal, payment: BigDecimal, extra: BigDecimal, anchor: LocalDate, maxPayments: Int): List[AmortizationRow] = {
    val r = annualRate / 12
    @scala.annotation.tailrec
    def loop(n: Int, bal: BigDecimal, acc: Vector[AmortizationRow]): List[AmortizationRow] =
      if (bal <= 0 || n > maxPayments) acc.toList
      else {
        val interest  = money(bal * r)
        val principal = (payment + extra - interest).min(bal).max(0)
        val next      = money(bal - principal)
        val row       = AmortizationRow(n, anchor.plusMonths(n.toLong), money(interest + principal), interest, money(principal), next)
        if (principal == 0) (acc :+ row).toList else loop(n + 1, next, acc :+ row)
      }
    loop(1, money(balance), Vector.empty)
  }

  def forLiability(l: Liability): List[AmortizationRow] =
    schedule(l.principal, l.annualRate, monthlyPayment(l.principal, l.annualRate, l.termMonths), l.extraMonthlyPayment, l.startDate, l.termMonths + 600)

  /** Balance on `date`: from the latest statement on or before it, else from origination. */
  def balanceOn(l: Liability, statements: List[LiabilityStatement], date: LocalDate): BigDecimal =
    if (date.isBefore(l.startDate)) BigDecimal(0)
    else {
      val payment = monthlyPayment(l.principal, l.annualRate, l.termMonths)
      val (anchor, bal) = statements.filter(!_.date.isAfter(date)).maxByOption(_.date)
        .fold((l.startDate, l.principal))(s => (s.date, s.balance))
      schedule(bal, l.annualRate, payment, l.extraMonthlyPayment, anchor, l.termMonths + 600)
        .takeWhile(!_.date.isAfter(date)).lastOption.fold(money(bal))(_.balance)
    }
}

// ---------- Vehicles ----------

/**
 * Declining-balance depreciation by vehicle class: year n loses the class's
 * n-th rate (scaled by the pace), 4% a year after the curve ends, and part
 * years lose a pro-rata share.
 */
object Depreciation {

  val TailRatePct = 4

  def value(purchasePrice: BigDecimal, purchased: LocalDate, cls: VehicleClass, pace: DepreciationPace, on: LocalDate): BigDecimal =
    if (!on.isAfter(purchased)) purchasePrice
    else {
      val days      = ChronoUnit.DAYS.between(purchased, on).toDouble
      val fullYears = (days / 365.25).toInt
      val partial   = days / 365.25 - fullYears
      def rate(year: Int) = cls.curve.lift(year).getOrElse(TailRatePct).toDouble / 100 * pace.multiplier.toDouble
      val afterFull = (0 until fullYears).foldLeft(purchasePrice.toDouble)((v, y) => v * (1 - rate(y)).max(0))
      BigDecimal(afterFull * (1 - rate(fullYears) * partial).max(0)).setScale(2, RoundingMode.HALF_EVEN)
    }
}
