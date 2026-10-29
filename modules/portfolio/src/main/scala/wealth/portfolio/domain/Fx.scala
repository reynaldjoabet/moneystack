package wealth.portfolio.domain

import wealth.domain.*

import java.time.LocalDate
import scala.collection.immutable.TreeMap

export wealth.platform.FxTable

/** One unit of `from` is worth `rate` units of `to` from `date` onwards. */
type FxRate = wealth.platform.FxQuote
val FxRate = wealth.platform.FxQuote

extension (fx: FxTable) {
  def convert(amount: BigDecimal, from: CurrencyCode, to: CurrencyCode, date: LocalDate): Either[PortfolioError, BigDecimal] =
    fx.convertOpt(amount, from, to, date).toRight(PortfolioError.MissingFxRate(from, to))
}

/** Close prices by asset; a date uses the latest close on or before it. */
final class PriceTable private (closes: Map[AssetId, TreeMap[LocalDate, BigDecimal]]) {

  def close(asset: AssetId, date: LocalDate): Option[BigDecimal] =
    closes.get(asset).flatMap(_.rangeTo(date).lastOption.map(_._2))
}

object PriceTable {
  val empty: PriceTable = new PriceTable(Map.empty)

  def apply(quotes: List[Quote]): PriceTable =
    new PriceTable(quotes.groupBy(_.assetId).view.mapValues(qs => TreeMap.from(qs.map(q => q.date -> q.close))).toMap)
}
