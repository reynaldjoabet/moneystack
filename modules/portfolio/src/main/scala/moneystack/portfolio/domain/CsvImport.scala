package moneystack.portfolio.domain

import moneystack.domain.*

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.LocalDate

/** One parsed activity row. `symbol` is resolved to an asset by the service. */
final case class ImportRow(
    line: Int,
    date: LocalDate,
    kind: ActivityType,
    symbol: Option[Symbol],
    quantity: Option[BigDecimal],
    unitPrice: Option[BigDecimal],
    amount: Option[BigDecimal],
    fee: BigDecimal,
    currency: CurrencyCode,
    external: Boolean,
    note: Option[Body]
) {

  /** Stable per account, so re-importing the same file adds nothing. */
  def key(account: InvestmentAccountId): String = {
    val raw = List(
      account.value.toString,
      date.toString,
      kind.key,
      symbol.fold("")(_.value),
      quantity.fold("")(_.bigDecimal.stripTrailingZeros.toPlainString),
      unitPrice.fold("")(_.bigDecimal.stripTrailingZeros.toPlainString),
      amount.fold("")(_.bigDecimal.stripTrailingZeros.toPlainString),
      fee.bigDecimal.stripTrailingZeros.toPlainString,
      currency.value
    ).mkString("|")
    MessageDigest.getInstance("SHA-256").digest(raw.getBytes(UTF_8)).map("%02x".format(_)).mkString
  }
}

/**
 * Activity CSV with a header row. Columns (any order, case-insensitive):
 * `date` (YYYY-MM-DD), `type`, `currency` required; `symbol`, `quantity`,
 * `unit_price` (or `price`), `amount`, `fee`, `external`, `note` optional.
 */
object CsvImport {

  final case class Parsed(rows: List[ImportRow], errors: List[String])

  def parse(csv: String): Parsed = {
    val lines = csv.linesIterator.zipWithIndex.filter(_._1.trim.nonEmpty).toList
    lines match {
      case Nil                 => Parsed(Nil, List("the file is empty"))
      case (header, _) :: body =>
        val cols    = split(header).map(_.trim.toLowerCase.replace(' ', '_'))
        val missing = List("date", "type", "currency").filterNot(cols.contains)
        if (missing.nonEmpty) Parsed(Nil, List(s"missing columns: ${missing.mkString(", ")}"))
        else {
          val results = body.map { case (text, idx) => row(cols, split(text), idx + 1) }
          Parsed(results.collect { case Right(r) => r }, results.collect { case Left(e) => e })
        }
    }
  }

  private def row(cols: List[String], cells: List[String], line: Int): Either[String, ImportRow] = {
    val m                                                     = cols.zip(cells.map(_.trim)).toMap.filter(_._2.nonEmpty)
    def dec(name: String): Either[String, Option[BigDecimal]] =
      m.get(name).traverseOpt(v =>
        scala.util.Try(BigDecimal(v.replace(",", ""))).toEither.left.map(_ => s"line $line: $name is not a number: $v")
      )
    for {
      date <- m.get("date").toRight(s"line $line: date is required")
        .flatMap(d => scala.util.Try(LocalDate.parse(d)).toEither.left.map(_ => s"line $line: invalid date $d"))
      kind <- m.get("type").toRight(s"line $line: type is required").flatMap(t => ActivityType.parse(t).left.map(e => s"line $line: $e"))
      currency <- m.get("currency").toRight(s"line $line: currency is required").flatMap(c =>
        CurrencyCode.either(c.toUpperCase).left.map(e => s"line $line: $e")
      )
      symbol <- m.get("symbol").traverseOpt(s => Symbol.either(s).left.map(e => s"line $line: invalid symbol $s ($e)"))
      qty    <- dec("quantity")
      price  <- dec("unit_price").flatMap(p => if (p.isDefined) Right(p) else dec("price"))
      amount <- dec("amount")
      fee    <- dec("fee")
      note   <- m.get("note").traverseOpt(n => Body.either(n.take(8000)).left.map(e => s"line $line: $e"))
    } yield ImportRow(
      line,
      date,
      kind,
      symbol,
      qty,
      price,
      amount,
      fee.getOrElse(BigDecimal(0)),
      currency,
      m.get("external").exists(v => Set("true", "yes", "1", "y").contains(v.toLowerCase)),
      note
    )
  }

  /** Splits a CSV line, honouring double quotes and doubled quotes inside them. */
  def split(line: String): List[String] = moneystack.platform.Csv.split(line)

  extension [A](o: Option[A]) {
    private def traverseOpt[B](f: A => Either[String, B]): Either[String, Option[B]] =
      o.fold[Either[String, Option[B]]](Right(None))(a => f(a).map(Some(_)))
  }
}
