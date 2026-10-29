package moneystack.accounting.service

import moneystack.domain.*
import moneystack.accounting.domain.*

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.math.BigDecimal.RoundingMode

/** Sub-ledger statements as pure functions. All totals are in the ledger's base currency. */
object TradeReports {

  // ---- receivables / payables aging ----

  final case class AgingBuckets(
      currentMinor: AmountMinor,
      days1To30Minor: AmountMinor,
      days31To60Minor: AmountMinor,
      days61To90Minor: AmountMinor,
      over90Minor: AmountMinor,
      totalMinor: AmountMinor
  )

  final case class AgingDocument(
      documentId: DocumentId,
      number: String,
      contactId: ContactId,
      dueDate: LocalDate,
      daysOverdue: Long,
      currency: CurrencyCode,
      openMinor: AmountMinor,
      openBaseMinor: AmountMinor
  )

  final case class AgingRow(contactId: ContactId, name: Title, buckets: AgingBuckets)

  final case class Aging(
      kind: DocumentKind,
      asOf: LocalDate,
      currency: CurrencyCode,
      rows: List[AgingRow],
      totals: AgingBuckets,
      documents: List[AgingDocument]
  )

  /** Buckets each open document by days past due as of `asOf`: current, 1-30, 31-60, 61-90, 90+. */
  def aging(kind: DocumentKind, asOf: LocalDate, currency: CurrencyCode, contacts: List[Contact], items: List[OpenItem]): Aging = {
    val open = items.filter(_.openMinor > 0L)
    val docs = open.map { i =>
      AgingDocument(
        i.documentId,
        i.number,
        i.contactId,
        i.dueDate,
        ChronoUnit.DAYS.between(i.dueDate, asOf).max(0L),
        i.currency,
        minor(i.openMinor),
        minor(i.openBaseMinor)
      )
    }
    val names = contacts.map(c => c.id -> c.name).toMap
    val rows  = docs.groupBy(_.contactId).toList.flatMap { case (id, ds) =>
      names.get(id).map(name => AgingRow(id, name, buckets(ds)))
    }.sortBy(_.name.value: String)
    Aging(kind, asOf, currency, rows, buckets(docs), docs.sortBy(d => (-d.daysOverdue, d.number)))
  }

  private def buckets(docs: List[AgingDocument]): AgingBuckets = {
    val sums = Array.fill(5)(0L)
    docs.foreach { d =>
      val i = d.daysOverdue match {
        case 0L            => 0
        case n if n <= 30L => 1
        case n if n <= 60L => 2
        case n if n <= 90L => 3
        case _             => 4
      }
      sums(i) += d.openBaseMinor.value
    }
    AgingBuckets(minor(sums(0)), minor(sums(1)), minor(sums(2)), minor(sums(3)), minor(sums(4)), minor(sums.sum))
  }

  // ---- sales tax liability ----

  final case class SalesTaxRow(
      taxRateId: TaxRateId,
      code: TaxCode,
      name: Title,
      rateBps: Bps,
      salesTaxableMinor: AmountMinor,
      taxCollectedMinor: AmountMinor,
      purchasesTaxableMinor: AmountMinor,
      taxPaidMinor: AmountMinor,
      netMinor: AmountMinor
  )

  final case class SalesTax(
      from: LocalDate,
      to: LocalDate,
      currency: CurrencyCode,
      rows: List[SalesTaxRow],
      totalCollectedMinor: AmountMinor,
      totalPaidMinor: AmountMinor,
      netPayableMinor: AmountMinor
  )

  /** Tax charged on invoices less tax recoverable on bills, per rate. */
  def salesTax(from: LocalDate, to: LocalDate, currency: CurrencyCode, rates: List[TaxRate], activity: List[TaxActivity]): SalesTax = {
    def of(rate: TaxRateId, kind: DocumentKind) =
      activity.find(a => a.taxRateId == rate && a.kind == kind).fold((0L, 0L))(a => (a.taxableBaseMinor, a.taxBaseMinor))

    val rows = rates.filter(r => activity.exists(_.taxRateId == r.id)).sortBy(_.code.value: String).map { r =>
      val (salesTaxable, collected) = of(r.id, DocumentKind.Invoice)
      val (purchTaxable, paid)      = of(r.id, DocumentKind.Bill)
      SalesTaxRow(
        r.id,
        r.code,
        r.name,
        r.rateBps,
        minor(salesTaxable),
        minor(collected),
        minor(purchTaxable),
        minor(paid),
        minor(collected - paid)
      )
    }
    val collected = rows.map(_.taxCollectedMinor.value: Long).sum
    val paid      = rows.map(_.taxPaidMinor.value: Long).sum
    SalesTax(from, to, currency, rows, minor(collected), minor(paid), minor(collected - paid))
  }

  // ---- inventory valuation ----

  final case class InventoryRow(
      itemId: ItemId,
      sku: Sku,
      name: Title,
      quantity: BigDecimal,
      valueMinor: AmountMinor,
      averageCostMinor: Option[BigDecimal]
  )

  final case class InventoryValuation(asOf: LocalDate, currency: CurrencyCode, rows: List[InventoryRow], totalValueMinor: AmountMinor)

  def inventoryValuation(asOf: LocalDate, currency: CurrencyCode, items: List[Item], stocks: List[Stock]): InventoryValuation = {
    val byItem = stocks.map(s => s.itemId -> s).toMap
    val rows   = items.filter(_.kind == ItemKind.Inventory).sortBy(_.sku.value: String).map { i =>
      val s   = byItem.getOrElse(i.id, Stock(i.id, BigDecimal(0), 0L))
      val avg = Option.when(s.quantity > 0)((BigDecimal(s.valueMinor) / s.quantity).setScale(4, RoundingMode.HALF_EVEN))
      InventoryRow(i.id, i.sku, i.name, s.quantity, minor(s.valueMinor), avg)
    }
    InventoryValuation(asOf, currency, rows, minor(rows.map(_.valueMinor.value: Long).sum))
  }

  private def minor(l: Long): AmountMinor = AmountMinor.assume(l)
}
