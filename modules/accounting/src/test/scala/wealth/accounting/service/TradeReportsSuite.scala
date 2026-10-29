package wealth.accounting.service

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.service.LedgerService.SystemAccounts

import java.time.{Instant, LocalDate}
import java.util.UUID

class TradeReportsSuite extends munit.FunSuite {

  private val usd  = CurrencyCode.USD
  private val asOf = LocalDate.of(2026, 6, 30)

  private def contact(name: String) =
    Contact(ContactId.assume(UUID.randomUUID()), ContactKind.Customer, Title.applyUnsafe(name), None, usd, None, active = true, Instant.EPOCH)

  private def open(c: Contact, number: String, due: LocalDate, total: Long, paid: Long) =
    OpenItem(DocumentId.assume(UUID.randomUUID()), number, c.id, due.minusDays(30), due, usd, total, paid, total, paid)

  test("aging buckets open amounts by days past due and skips settled documents") {
    val acme = contact("Acme")
    val zed  = contact("Zed")
    val items = List(
      open(acme, "INV-1", asOf.plusDays(5), 1_000, 0),       // current
      open(acme, "INV-2", asOf.minusDays(30), 2_000, 500),   // 1-30
      open(acme, "INV-3", asOf.minusDays(31), 3_000, 0),     // 31-60
      open(zed, "INV-4", asOf.minusDays(90), 4_000, 0),      // 61-90
      open(zed, "INV-5", asOf.minusDays(91), 5_000, 0),      // 90+
      open(zed, "INV-6", asOf.minusDays(200), 6_000, 6_000)  // paid
    )
    val report = TradeReports.aging(DocumentKind.Invoice, asOf, usd, List(zed, acme), items)

    def v(a: AmountMinor): Long = a.value
    assertEquals(report.rows.map(_.name.value: String), List("Acme", "Zed"))
    val t = report.totals
    assertEquals(List(t.currentMinor, t.days1To30Minor, t.days31To60Minor, t.days61To90Minor, t.over90Minor).map(v),
                 List(1_000L, 1_500L, 3_000L, 4_000L, 5_000L))
    assertEquals(v(t.totalMinor), 14_500L)
    assertEquals(v(report.rows.head.buckets.totalMinor), 5_500L)
    assertEquals(report.documents.map(_.number).head, "INV-5")
    assert(!report.documents.exists(_.number == "INV-6"))
  }

  test("sales tax nets collected against recoverable per rate") {
    val vat = TaxRate(TaxRateId.assume(UUID.randomUUID()), TaxCode.applyUnsafe("VAT20"), Title.applyUnsafe("VAT 20%"),
                      Bps.assume(2_000), active = true, Instant.EPOCH)
    val unused = vat.copy(id = TaxRateId.assume(UUID.randomUUID()), code = TaxCode.applyUnsafe("ZERO"))
    val report = TradeReports.salesTax(asOf.withDayOfYear(1), asOf, usd, List(unused, vat), List(
      TaxActivity(DocumentKind.Invoice, vat.id, 50_000, 10_000),
      TaxActivity(DocumentKind.Bill, vat.id, 20_000, 4_000)
    ))
    assertEquals(report.rows.map(_.code.value: String), List("VAT20"))
    assertEquals(report.rows.head.netMinor.value: Long, 6_000L)
    assertEquals(report.netPayableMinor.value: Long, 6_000L)
  }

  test("inventory valuation lists inventory items with average cost") {
    def item(sku: String, kind: ItemKind) =
      Item(ItemId.assume(UUID.randomUUID()), Sku.applyUnsafe(sku), Title.applyUnsafe(sku), kind, None, None,
           SystemAccounts.Sales.id, SystemAccounts.CostOfGoodsSold.id,
           Option.when(kind == ItemKind.Inventory)(SystemAccounts.Inventory.id), active = true, Instant.EPOCH)
    val widget  = item("WIDGET", ItemKind.Inventory)
    val gadget  = item("GADGET", ItemKind.Inventory)
    val service = item("SETUP", ItemKind.Service)
    val report = TradeReports.inventoryValuation(asOf, usd, List(widget, gadget, service),
                                                 List(Stock(widget.id, BigDecimal(3), 1_000L)))
    assertEquals(report.rows.map(_.sku.value: String), List("GADGET", "WIDGET"))
    assertEquals(report.rows(1).averageCostMinor, Some(BigDecimal("333.3333")))
    assertEquals(report.rows.head.averageCostMinor, None)
    assertEquals(report.totalValueMinor.value: Long, 1_000L)
  }
}
