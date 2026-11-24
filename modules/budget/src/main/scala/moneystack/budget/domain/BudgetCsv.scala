package moneystack.budget.domain

import moneystack.domain.*
import moneystack.platform.Csv

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.LocalDate

/** A parsed import row; `category` and `payee` are names, resolved by the service. */
final case class CsvRow(
    line: Int,
    date: LocalDate,
    kind: TxKind,
    amountMinor: Long,
    title: Title,
    category: Option[String],
    payee: Option[String],
    note: Option[Body]
) {

  /** Stable per wallet, so re-importing a statement adds nothing. */
  def key(wallet: WalletId): String = {
    val raw =
      List(wallet.value.toString, date.toString, kind.key, amountMinor.toString, (title.value: String).trim.toLowerCase).mkString("|")
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
      case Nil                 => Parsed(Nil, List("the file is empty"))
      case (header, _) :: body =>
        val cols     = Csv.split(header).map(_.trim.toLowerCase.replace(' ', '_'))
        val titleCol = if (cols.contains("title")) "title" else "description"
        val missing  = List("date", "amount", titleCol).filterNot(cols.contains)
        if (missing.nonEmpty) Parsed(Nil, List(s"missing columns: ${missing.mkString(", ")}"))
        else {
          val results = body.map { case (text, idx) => row(cols, Csv.split(text), idx + 1, titleCol) }
          Parsed(results.collect { case Right(r) => r }, results.collect { case Left(e) => e })
        }
    }

  private val Dashes  = "\u2010\u2011\u2012\u2013\u2014\u2015\u2212\uFE63\uFF0D"
  private val Symbols = "$€£¥₴₽\u00A0 "
  private val Plain   = "(\\d+(\\.\\d+)?|\\.\\d+)".r

  /** `1,234,567` (or with dots): a first group of 1 to 3 digits, then groups of exactly 3. */
  private def grouped(s: String, sep: Char): Option[String] = {
    val parts = s.split(java.util.regex.Pattern.quote(sep.toString), -1).toList
    Option.when(
      parts.headOption.exists(h => h.nonEmpty && h.length <= 3 && h.forall(_.isDigit)) &&
        parts.tail.forall(p => p.length == 3 && p.forall(_.isDigit))
    )(parts.mkString)
  }

  /**
   * An amount as banks export it: `-1234.56`, `+12`, `(1,234.56)` (negative), `1234.56 DR` (negative) or `CR`,
   * currency symbols, a Unicode minus, `1.234,56` and `1,234.56`, and repeated separators grouping thousands
   * (`3,250,000`, `1.500.000`). A lone comma followed by exactly 3 digits groups thousands (`1,234` is 1234), as
   * this importer always read it; followed by 1 or 2 digits it is the decimal point (`12,5`). `None` for anything
   * else, including a `DR`/`CR` suffix next to a sign, which contradicts it.
   */
  def amount(raw: String): Option[BigDecimal] = {
    var s = raw.trim
    if (s.nonEmpty && Dashes.contains(s.head)) s = "-" + s.tail
    val suffix = "(?i)\\s*(DR|CR)\\.?$".r.findFirstMatchIn(s)
    val debit  = suffix.exists(_.group(1).equalsIgnoreCase("DR"))
    suffix.foreach(m => s = s.substring(0, m.start))
    val parens = s.length >= 2 && s.head == '(' && s.last == ')'
    if (parens) s = s.substring(1, s.length - 1)
    def sign(): Option[Char] = s.headOption.filter(c => c == '-' || c == '+').map { c => s = s.tail; c }
    val before               = sign()
    s = s.filterNot(c => Symbols.contains(c))
    val after   = sign()
    val negated = List(before, after).flatten.count(_ == '-') % 2 == 1
    if (suffix.isDefined && (parens || before.contains('-') || after.contains('-'))) None
    else {
      val commas  = s.count(_ == ',')
      val periods = s.count(_ == '.')
      val number  =
        if (commas > 0 && periods > 0) {
          // Both: the last one is the decimal point, the other groups thousands.
          if (s.lastIndexOf(',') > s.lastIndexOf('.')) Some(s.replace(".", "").replace(',', '.')) else Some(s.replace(",", ""))
        } else if (commas > 1) grouped(s, ',')
        else if (periods > 1) grouped(s, '.')
        else if (commas == 1) {
          val decimals = s.length - s.indexOf(',') - 1
          if (decimals == 3) grouped(s, ',') else Some(s.replace(',', '.'))
        } else Some(s)
      number.filter(Plain.matches).map(BigDecimal(_)).map(n => if (debit || (parens != negated)) -n else n)
    }
  }

  private def row(cols: List[String], cells: List[String], line: Int, titleCol: String): Either[String, CsvRow] = {
    val m = cols.zip(cells.map(_.trim)).toMap.filter(_._2.nonEmpty)
    for {
      date <- m.get("date").toRight(s"line $line: date is required")
        .flatMap(d => scala.util.Try(LocalDate.parse(d)).toEither.left.map(_ => s"line $line: invalid date $d"))
      raw <- m.get("amount").toRight(s"line $line: amount is required")
        .flatMap(a => amount(a).toRight(s"line $line: amount is not a number: $a"))
      _    <- Either.cond(raw != 0, (), s"line $line: amount must not be zero")
      kind <- m.get("type").fold[Either[String, TxKind]](Right(if (raw < 0) TxKind.Expense else TxKind.Income))(t =>
        TxKind.parse(t).left.map(e => s"line $line: $e").filterOrElse(_ != TxKind.Transfer, s"line $line: transfers can't be imported")
      )
      title <- m.get(titleCol).toRight(s"line $line: $titleCol is required").flatMap(t =>
        Title.either(t.take(200)).left.map(e => s"line $line: $e")
      )
    } yield CsvRow(
      line,
      date,
      kind,
      (raw.abs * 100).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLong,
      title,
      m.get("category"),
      m.get("payee"),
      m.get("note").flatMap(n => Body.option(n.take(8000)))
    )
  }

  val ExportHeader: List[String] = List("date", "type", "wallet", "amount", "currency", "category", "payee", "title", "note", "tags")

  /** One CSV line per transaction; amounts in major units, transfers shown from the source wallet. */
  def write(
      txs: List[Transaction],
      wallets: Map[WalletId, Wallet],
      categories: Map[CategoryId, Category],
      payees: Map[PayeeId, Payee],
      tags: Map[TagId, Tag]
  ): String = {
    def major(m: Long) = (BigDecimal(m) / 100).setScale(2).toString
    val lines          = txs.sortBy(t => (t.date, t.createdAt)).map { t =>
      val w = wallets.get(t.walletId)
      Csv.row(List(
        t.date.toString,
        t.kind.key,
        w.fold("")(x => x.name.value: String),
        major(t.amountMinor),
        w.fold("")(x => x.currency.value: String),
        t.categoryId.flatMap(categories.get).fold("")(c => c.name.value: String),
        t.payeeId.flatMap(payees.get).fold("")(p => p.name.value: String),
        t.title.value,
        t.note.fold("")(n => n.value: String),
        t.tags.flatMap(tags.get).map(x => x.name.value: String).mkString(";")
      ))
    }
    (Csv.row(ExportHeader) :: lines).mkString("\n") + "\n"
  }
}
