package moneystack.accounting.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID
import scala.math.BigDecimal.RoundingMode

// ---------- Contacts ----------

enum ContactKind(val key: String) {
  case Customer extends ContactKind("customer")
  case Vendor   extends ContactKind("vendor")
}
object ContactKind {
  def parse(s: String): Either[String, ContactKind] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown contact kind: $s")
  def render(k: ContactKind): String = k.key
}

/** A customer or vendor. Documents are issued in the contact's currency. `userId` links an app user. */
final case class Contact(
    id: ContactId,
    kind: ContactKind,
    name: Title,
    email: Option[Email],
    currency: CurrencyCode,
    userId: Option[UserId],
    active: Boolean,
    createdAt: Instant
)

// ---------- Tax ----------

/** A sales/purchase tax applied exclusive of line amounts. */
final case class TaxRate(id: TaxRateId, code: TaxCode, name: Title, rateBps: Bps, active: Boolean, createdAt: Instant)

// ---------- Items & inventory ----------

enum ItemKind(val key: String) {
  case Service      extends ItemKind("service")
  case NonInventory extends ItemKind("non_inventory")
  case Inventory    extends ItemKind("inventory")
}
object ItemKind {
  def parse(s: String): Either[String, ItemKind] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown item kind: $s")
  def render(k: ItemKind): String = k.key
}

/**
 * `expenseAccountId` is cost of goods sold for inventory items and the
 * purchase expense otherwise; `inventoryAccountId` is set for inventory items only.
 */
final case class Item(
    id: ItemId,
    sku: Sku,
    name: Title,
    kind: ItemKind,
    sellPriceMinor: Option[PriceMinor],
    costPriceMinor: Option[PriceMinor],
    incomeAccountId: AccountId,
    expenseAccountId: AccountId,
    inventoryAccountId: Option[AccountId],
    active: Boolean,
    createdAt: Instant
)

/** On-hand quantity and its carrying value in base currency (moving average cost). */
final case class Stock(itemId: ItemId, quantity: BigDecimal, valueMinor: Long) {

  /** Cost of issuing `qty`; issuing everything takes the remaining value exactly. */
  def costOf(qty: BigDecimal): Long =
    if (qty == quantity) valueMinor
    else (BigDecimal(valueMinor) * qty / quantity).setScale(0, RoundingMode.HALF_EVEN).toLongExact

  def add(qty: BigDecimal, value: Long): Stock = copy(quantity = quantity + qty, valueMinor = valueMinor + value)
}

/** A signed change to stock. History is append-only; `item_stock` holds the running position. */
final case class InventoryMove(
    itemId: ItemId,
    date: LocalDate,
    quantity: BigDecimal,
    valueMinor: Long,
    source: JournalSource,
    sourceId: UUID
)

final case class InventoryAdjustment(
    id: AdjustmentId,
    itemId: ItemId,
    date: LocalDate,
    quantity: BigDecimal,
    valueMinor: Long,
    accountId: AccountId,
    reason: Body,
    entryId: JournalEntryId,
    createdBy: Option[UserId],
    createdAt: Instant
)

// ---------- Invoices & bills ----------

/**
 * Invoices (sales, receivable from customers) and bills (purchases, payable to
 * vendors) mirror each other; this fixes the sides and system accounts.
 */
enum DocumentKind(val key: String, val contactKind: ContactKind, val prefix: String, val settlementPrefix: String) {
  case Invoice extends DocumentKind("invoice", ContactKind.Customer, "INV", "RCPT")
  case Bill    extends DocumentKind("bill", ContactKind.Vendor, "BILL", "PAY")

  /** Side of the receivable/payable line when the document is posted. */
  def controlSide: Side = this match {
    case Invoice => Side.Debit
    case Bill    => Side.Credit
  }

  def documentSource: JournalSource = this match {
    case Invoice => JournalSource.Invoice
    case Bill    => JournalSource.Bill
  }

  def settlementSource: JournalSource = this match {
    case Invoice => JournalSource.Receipt
    case Bill    => JournalSource.BillPayment
  }
}
object DocumentKind {
  def parse(s: String): Either[String, DocumentKind] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown document kind: $s")
  def render(k: DocumentKind): String = k.key
}

enum DocumentStatus {
  case Open, PartiallyPaid, Paid, Void
}

/** Amounts are in the document currency; `taxBps` snapshots the rate at issue. */
final case class DocumentLine(
    lineNo: Int,
    itemId: Option[ItemId],
    description: Option[Body],
    quantity: Quantity,
    unitPriceMinor: PriceMinor,
    accountId: AccountId,
    taxRateId: Option[TaxRateId],
    taxBps: Option[Bps],
    amountMinor: AmountMinor,
    taxMinor: AmountMinor
)

/**
 * `controlBaseMinor` is what the receivable/payable line was booked at in base
 * currency; settlements draw it down exactly so fully paid documents leave no
 * rounding residue.
 */
final case class Document(
    id: DocumentId,
    kind: DocumentKind,
    number: String,
    contactId: ContactId,
    date: LocalDate,
    dueDate: LocalDate,
    currency: CurrencyCode,
    exchangeRate: ExchangeRate,
    reference: Option[JournalRef],
    memo: Option[Body],
    subtotalMinor: AmountMinor,
    taxMinor: AmountMinor,
    totalMinor: PriceMinor,
    controlBaseMinor: AmountMinor,
    entryId: JournalEntryId,
    voidedOn: Option[LocalDate],
    createdBy: Option[UserId],
    createdAt: Instant
)

final case class DocumentDetails(document: Document, lines: List[DocumentLine], paidMinor: AmountMinor, status: DocumentStatus)

// ---------- Receipts & bill payments ----------

/** `baseMinor` is the allocation at the settlement rate, `documentBaseMinor` at the document's. */
final case class Allocation(documentId: DocumentId, amountMinor: PriceMinor, baseMinor: AmountMinor, documentBaseMinor: AmountMinor)

/** A customer receipt (kind = Invoice) or vendor payment (kind = Bill), allocated to open documents. */
final case class Settlement(
    id: SettlementId,
    kind: DocumentKind,
    number: String,
    contactId: ContactId,
    date: LocalDate,
    currency: CurrencyCode,
    exchangeRate: ExchangeRate,
    accountId: AccountId,
    reference: Option[JournalRef],
    memo: Option[Body],
    totalMinor: PriceMinor,
    entryId: JournalEntryId,
    fxEntryId: Option[JournalEntryId],
    voidedOn: Option[LocalDate],
    createdBy: Option[UserId],
    createdAt: Instant
)

final case class SettlementDetails(settlement: Settlement, allocations: List[Allocation])

/** A document's position as of a date, for aging (amounts in document currency unless `Base`). */
final case class OpenItem(
    documentId: DocumentId,
    number: String,
    contactId: ContactId,
    date: LocalDate,
    dueDate: LocalDate,
    currency: CurrencyCode,
    totalMinor: Long,
    paidMinor: Long,
    controlBaseMinor: Long,
    paidBaseMinor: Long
) {
  def openMinor: Long     = totalMinor - paidMinor
  def openBaseMinor: Long = controlBaseMinor - paidBaseMinor
}

/** Taxable amount and tax per rate and document kind over a period, in base currency. */
final case class TaxActivity(kind: DocumentKind, taxRateId: TaxRateId, taxableBaseMinor: Long, taxBaseMinor: Long)

// ---------- Rules ----------

object Trade {

  def lineAmount(quantity: BigDecimal, unitPriceMinor: Long): Long =
    (quantity * unitPriceMinor).setScale(0, RoundingMode.HALF_EVEN).toLongExact

  def lineTax(amountMinor: Long, rate: Option[Bps]): Long =
    rate.fold(0L)(bps => (BigDecimal(amountMinor) * bps.value / 10_000).setScale(0, RoundingMode.HALF_EVEN).toLongExact)

  /**
   * Journal lines for a document: the receivable/payable first, then one line
   * per non-zero document line, then one per tax rate (in order of appearance).
   */
  def postingLines(
      kind: DocumentKind,
      contactId: ContactId,
      lines: List[DocumentLine],
      controlAccount: AccountId,
      taxAccount: AccountId
  ): List[JournalLine] = {
    val lineSide = kind.controlSide.opposite
    val total    = lines.map(l => l.amountMinor.value + l.taxMinor.value).sum
    val taxes    = lines.flatMap(l => l.taxRateId.map(_ -> (l.taxMinor.value: Long)))
    val taxSums  = taxes.map(_._1).distinct.map(id => taxes.filter(_._1 == id).map(_._2).sum)

    Ledger.line(controlAccount, kind.controlSide, total, Some(contactId)).toList ++
      lines.flatMap(l => Ledger.line(l.accountId, lineSide, l.amountMinor.value, None)) ++
      taxSums.flatMap(sum => Ledger.line(taxAccount, lineSide, sum, None))
  }

  /** Base-currency value of each non-zero document line, read back from the posted entry. */
  def lineBases(posted: List[PostedLine], lines: List[DocumentLine]): Map[Int, Long] =
    lines.filter(_.amountMinor.value != 0L)
      .zip(posted.drop(1))
      .map { case (l, p) => l.lineNo -> (p.baseAmountMinor.value: Long) }
      .toMap

  def status(d: Document, paidMinor: Long): DocumentStatus =
    if (d.voidedOn.isDefined) DocumentStatus.Void
    else if (paidMinor >= d.totalMinor.value) DocumentStatus.Paid
    else if (paidMinor > 0L) DocumentStatus.PartiallyPaid
    else DocumentStatus.Open

  /**
   * The share of a document's booked base amount that an allocation clears.
   * The allocation that completes payment takes whatever is left, so the
   * receivable/payable nets to exactly zero.
   */
  def documentBase(d: Document, paidMinor: Long, paidBaseMinor: Long, amountMinor: Long): Long =
    if (paidMinor + amountMinor == d.totalMinor.value) d.controlBaseMinor.value - paidBaseMinor
    else Ledger.convert(amountMinor, d.exchangeRate)

  /**
   * Realised exchange difference on a settlement, in base currency. `diff` is
   * the sum of (base at settlement rate - base at document rate); positive is
   * a gain on receipts and a loss on payments.
   */
  def fxLines(kind: DocumentKind, contactId: ContactId, diff: Long, controlAccount: AccountId, fxAccount: AccountId): List[JournalLine] =
    Ledger.line(controlAccount, kind.controlSide, diff, Some(contactId)).toList ++
      Ledger.line(fxAccount, kind.controlSide.opposite, diff, None).toList
}
