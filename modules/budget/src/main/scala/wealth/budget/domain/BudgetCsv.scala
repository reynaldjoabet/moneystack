package wealth.budget.domain

import wealth.domain.*
import wealth.platform.Csv

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.LocalDate

/** A parsed import row; `category` and `payee` are names, resolved by the service. */
final case class CsvRow(line: Int, date: LocalDate, kind: TxKind, amountMinor: Long, title: Title, category: Option[String],
                        payee: Option[String], note: Option[Body]) {

  /** Stable per wallet, so re-importing a statement adds nothing. */
  def key(wallet: WalletId): String = {
    val raw = List(wallet.value.toString, date.toString, kind.key, amountMinor.toString, (title.value: String).trim.toLowerCase).mkString("|")
    MessageDigest.getInstance("SHA-256").digest(raw.getBytes(UTF_8)).map("%02x".format(_)).mkString
  }
}

/**
 * Transaction CSV. Import needs `date`, `amount` and `title` (or
 * `description`); `type` is income/expense, else the amount's sign decides.
 * `category`, `payee` and `note` are optional. Amounts are in major units.
 */
object BudgetCsv {

  final case class Parsed(rows: List[CsvRow], errors: List[String])

  def parse(csv: String): Parsed =
    csv.linesIterator.zipWithIndex.filter(_._1.trim.nonEmpty).toList match {
      case Nil => Parsed(Nil, List("the file is empty"))
      case (header, _) :: body =>
        val cols    = Csv.split(header).map(_.trim.toLowerCase.replace(' ', '_'))
        val titleCol = if (cols.contains("title")) "title" else "description"
        val missing = List("date", "amount", titleCol).filterNot(cols.contains)
        if (missing.nonEmpty) Parsed(Nil, List(s"missing columns: ${missing.mkString(", ")}"))
        else {
          val results = body.map { case (text, idx) => row(cols, Csv.split(text), idx + 1, titleCol) }
          Parsed(results.collect { case Right(r) => r }, results.collect { case Left(e) => e })
        }
    }

  private def row(cols: List[String], cells: List[String], line: Int, titleCol: String): Either[String, CsvRow] = {
    val m = cols.zip(cells.map(_.trim)).toMap.filter(_._2.nonEmpty)
    for {
      date   <- m.get("date").toRight(s"line $line: date is required")
                  .flatMap(d => scala.util.Try(LocalDate.parse(d)).toEither.left.map(_ => s"line $line: invalid date $d"))
      raw    <- m.get("amount").toRight(s"line $line: amount is required")
                  .flatMap(a => scala.util.Try(BigDecimal(a.replace(",", ""))).toEither.left.map(_ => s"line $line: amount is not a number: $a"))
      _      <- Either.cond(raw != 0, (), s"line $line: amount must not be zero")
      kind   <- m.get("type").fold[Either[String, TxKind]](Right(if (raw < 0) TxKind.Expense else TxKind.Income))(t =>
                  TxKind.parse(t).left.map(e => s"line $line: $e").filterOrElse(_ != TxKind.Transfer, s"line $line: transfers can't be imported"))
      title  <- m.get(titleCol).toRight(s"line $line: $titleCol is required").flatMap(t => Title.either(t.take(200)).left.map(e => s"line $line: $e"))
    } yield CsvRow(line, date, kind, (raw.abs * 100).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLong, title,
                   m.get("category"), m.get("payee"), m.get("note").flatMap(n => Body.option(n.take(8000))))
  }

  val ExportHeader: List[String] = List("date", "type", "wallet", "amount", "currency", "category", "payee", "title", "note", "tags")

  /** One CSV line per transaction; amounts in major units, transfers shown from the source wallet. */
  def write(txs: List[Transaction], wallets: Map[WalletId, Wallet], categories: Map[CategoryId, Category],
             payees: Map[PayeeId, Payee], tags: Map[TagId, Tag]): String = {
    def major(m: Long) = (BigDecimal(m) / 100).setScale(2).toString
    val lines = txs.sortBy(t => (t.date, t.createdAt)).map { t =>
      val w = wallets.get(t.walletId)
      Csv.row(List(
        t.date.toString, t.kind.key, w.fold("")(x => x.name.value: String), major(t.amountMinor), w.fold("")(x => x.currency.value: String),
        t.categoryId.flatMap(categories.get).fold("")(c => c.name.value: String), t.payeeId.flatMap(payees.get).fold("")(p => p.name.value: String),
        t.title.value, t.note.fold("")(n => n.value: String), t.tags.flatMap(tags.get).map(x => x.name.value: String).mkString(";")
      ))
    }
    (Csv.row(ExportHeader) :: lines).mkString("\n") + "\n"
  }
}
