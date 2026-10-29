package moneystack.platform

import moneystack.domain.CurrencyCode

import java.time.LocalDate
import scala.collection.immutable.TreeMap

/** One unit of `from` is worth `rate` units of `to` from `date` onwards. */
final case class FxQuote(from: CurrencyCode, to: CurrencyCode, date: LocalDate, rate: BigDecimal)

/**
 * Exchange rates by pair and date. A pair converts directly or through its
 * inverse; a date uses the latest rate on or before it, falling back to the
 * earliest rate after it when history starts later.
 */
final class FxTable private (rates: Map[(CurrencyCode, CurrencyCode), TreeMap[LocalDate, BigDecimal]], unit: Boolean) {

  def rate(from: CurrencyCode, to: CurrencyCode, date: LocalDate): Option[BigDecimal] =
    if (from == to || unit) Some(BigDecimal(1))
    else
      FxTable.at(rates.get((from, to)), date)
        .orElse(FxTable.at(rates.get((to, from)), date).filter(_ != 0).map(BigDecimal(1) / _))

  def convertOpt(amount: BigDecimal, from: CurrencyCode, to: CurrencyCode, date: LocalDate): Option[BigDecimal] =
    rate(from, to, date).map(amount * _)
}

object FxTable {

  val empty: FxTable = new FxTable(Map.empty, unit = false)

  /** Every rate is 1: for checks that only care about quantities, such as validating an edit. */
  val unity: FxTable = new FxTable(Map.empty, unit = true)

  def apply(quotes: List[FxQuote]): FxTable =
    new FxTable(quotes.groupBy(q => (q.from, q.to)).view.mapValues(qs => TreeMap.from(qs.map(q => q.date -> q.rate))).toMap, unit = false)

  private def at(series: Option[TreeMap[LocalDate, BigDecimal]], date: LocalDate): Option[BigDecimal] =
    series.flatMap(s => s.rangeTo(date).lastOption.orElse(s.headOption).map(_._2))
}
