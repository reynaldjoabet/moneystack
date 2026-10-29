package moneystack.portfolio.domain

import moneystack.domain.*

import java.time.{LocalDate, YearMonth}
import scala.math.BigDecimal.RoundingMode

final case class Slice(category: String, value: BigDecimal, pct: BigDecimal)

final case class DriftRow(category: String, value: BigDecimal, actualPct: BigDecimal, targetPct: BigDecimal, driftPct: BigDecimal)

/** `trade` is what to buy (positive) or sell (negative) to reach the target. */
final case class RebalanceRow(category: String, current: BigDecimal, target: BigDecimal, trade: BigDecimal)

enum RebalanceMode(val key: String) {
  /** Only new cash is placed, into underweight categories in proportion to their shortfall. */
  case BuyOnly extends RebalanceMode("buy_only")
  /** Buys and sells to land exactly on target. */
  case Full    extends RebalanceMode("full")
}
object RebalanceMode extends Keyed[RebalanceMode] { def key(a: RebalanceMode) = a.key }

final case class IncomeSummary(
    total: BigDecimal,
    dividends: BigDecimal,
    interest: BigDecimal,
    byMonth: List[(YearMonth, BigDecimal)],
    byAsset: List[(AssetId, BigDecimal)],
    trailingTwelveMonths: BigDecimal
)

final case class LimitProgress(limit: ContributionLimit, from: LocalDate, to: LocalDate, contributed: BigDecimal, remaining: BigDecimal, usedPct: BigDecimal)

object Analytics {

  private def money(x: BigDecimal) = x.setScale(2, RoundingMode.HALF_EVEN)
  private def pct(part: BigDecimal, whole: BigDecimal) =
    if (whole == 0) BigDecimal(0) else (part / whole * 100).setScale(4, RoundingMode.HALF_EVEN)

  val AssetKindTaxonomy = "asset_kind"
  val CurrencyTaxonomy  = "currency"
  val CashCategory      = "Cash"
  val Unclassified      = "Unclassified"

  /**
   * Splits holdings (and cash) by a taxonomy. `asset_kind` and `currency` are
   * built in; others use the assets' weighted classifications, with any
   * unweighted remainder going to "Unclassified".
   */
  def allocation(holdings: List[HoldingView], cash: BigDecimal, taxonomy: String, classifications: List[Classification]): List[Slice] = {
    val byAsset = classifications.filter(_.taxonomy.value == taxonomy).groupBy(_.assetId)
    val parts = holdings.flatMap { h =>
      taxonomy match {
        case AssetKindTaxonomy => List(h.kind.key -> h.marketValueBase)
        case CurrencyTaxonomy  => List(h.currency.value -> h.marketValueBase)
        case _ =>
          val cs     = byAsset.getOrElse(h.assetId, Nil)
          val shares = cs.map(c => c.category.value -> h.marketValueBase * c.weightBps.value / 10_000)
          val rest   = h.marketValueBase - shares.map(_._2).sum
          if (rest > 0) shares :+ (Unclassified -> rest) else shares
      }
    } ++ Option.when(cash != 0)(CashCategory -> cash)
    val total = parts.map(_._2).sum
    parts.groupMapReduce(_._1)(_._2)(_ + _).toList
      .map { case (k, v) => Slice(k, money(v), pct(v, total)) }
      .sortBy(s => -s.value)
  }

  def validateTargets(targets: List[(String, Int)]): Either[PortfolioError, Unit] =
    if (targets.isEmpty) Right(())
    else if (targets.map(_._1).distinct.size != targets.size) Left(PortfolioError.InvalidTargets("each category may appear once"))
    else if (targets.map(_._2).sum != 10_000) Left(PortfolioError.InvalidTargets(s"weights must sum to 10000 bps, got ${targets.map(_._2).sum}"))
    else Right(())

  def drift(slices: List[Slice], targets: List[AllocationTarget]): List[DriftRow] = {
    val total   = slices.map(_.value).sum
    val actual  = slices.map(s => s.category -> s.value).toMap
    val wanted  = targets.map(t => (t.category.value: String) -> BigDecimal(t.weightBps.value) / 100).toMap
    (actual.keySet ++ wanted.keySet).toList.sorted.map { c =>
      val a = pct(actual.getOrElse(c, BigDecimal(0)), total)
      val t = wanted.getOrElse(c, BigDecimal(0)).setScale(4)
      DriftRow(c, actual.getOrElse(c, BigDecimal(0)), a, t, a - t)
    }
  }

  def rebalance(slices: List[Slice], targets: List[AllocationTarget], newCash: BigDecimal, mode: RebalanceMode): List[RebalanceRow] = {
    val current: Map[String, BigDecimal] = slices.map(s => s.category -> s.value).toMap
    val total   = current.values.sum + newCash
    val goal    = targets.map(t => (t.category.value: String) -> total * t.weightBps.value / 10_000).toMap
    val cats    = (current.keySet ++ goal.keySet).toList.sorted
    mode match {
      case RebalanceMode.Full =>
        cats.map(c => RebalanceRow(c, money(current.getOrElse(c, BigDecimal(0))), money(goal.getOrElse(c, BigDecimal(0))), money(goal.getOrElse(c, BigDecimal(0)) - current.getOrElse(c, BigDecimal(0)))))
      case RebalanceMode.BuyOnly =>
        val shortfall = cats.map(c => c -> (goal.getOrElse(c, BigDecimal(0)) - current.getOrElse(c, BigDecimal(0))).max(0)).toMap
        val need      = shortfall.values.sum
        cats.map { c =>
          val trade = if (need == 0) BigDecimal(0) else newCash * shortfall(c) / need
          RebalanceRow(c, money(current.getOrElse(c, BigDecimal(0))), money(goal.getOrElse(c, BigDecimal(0))), money(trade))
        }
    }
  }

  def income(events: List[IncomeEvent], from: LocalDate, to: LocalDate, asOf: LocalDate): IncomeSummary = {
    val in = events.filter(e => !e.date.isBefore(from) && !e.date.isAfter(to))
    def sum(es: List[IncomeEvent]) = money(es.map(_.amountBase).sum)
    IncomeSummary(
      total     = sum(in),
      dividends = sum(in.filter(_.kind == ActivityType.Dividend)),
      interest  = sum(in.filter(_.kind == ActivityType.Interest)),
      byMonth   = in.groupBy(e => YearMonth.from(e.date)).toList.sortBy(_._1).map { case (m, es) => m -> sum(es) },
      byAsset   = in.flatMap(e => e.assetId.map(_ -> e)).groupBy(_._1).toList.map { case (a, es) => a -> sum(es.map(_._2)) }.sortBy(-_._2),
      trailingTwelveMonths = sum(events.filter(e => e.date.isAfter(asOf.minusYears(1)) && !e.date.isAfter(asOf)))
    )
  }

  /**
   * Contributions toward a limit: deposits and external transfers into its
   * accounts within the window (the calendar year unless dates are given).
   */
  def limitProgress(limit: ContributionLimit, books: List[Book], fx: FxTable, base: CurrencyCode): Either[PortfolioError, LimitProgress] = {
    val from = limit.startDate.getOrElse(LocalDate.of(limit.year, 1, 1))
    val to   = limit.endDate.getOrElse(LocalDate.of(limit.year, 12, 31))
    val flows = books.filter(b => limit.accountIds.contains(b.accountId))
      .flatMap(_.flows.filter(f => !(f.transfer && f.counterAccount.isDefined) && f.amountBase > 0 && !f.date.isBefore(from) && !f.date.isAfter(to)))
    flows.foldLeft[Either[PortfolioError, BigDecimal]](Right(BigDecimal(0))) { (acc, f) =>
      acc.flatMap(s => fx.convert(f.amountBase, base, limit.currency, f.date).map(s + _))
    }.map { contributed =>
      LimitProgress(limit, from, to, money(contributed), money((limit.limitAmount - contributed).max(0)), pct(contributed, limit.limitAmount))
    }
  }

}
