package wealth.accounting.it

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.service.{CatalogService, TradeService}
import wealth.accounting.service.LedgerService.SystemAccounts
import wealth.accounting.service.TradeService.{NewAllocation, NewDocument, NewLine, NewSettlement}

import cats.effect.*

import java.time.LocalDate

class TradeIntegrationSuite extends PostgresSuite {

  private def d(m: Int, day: Int) = LocalDate.of(2026, m, day)
  private def q(s: String)        = Quantity.assume(BigDecimal(s))
  private def p(l: Long)          = PriceMinor.assume(l)
  private val farFuture           = LocalDate.of(2030, 1, 1)

  private def contact(kind: ContactKind, name: String, currency: CurrencyCode = CurrencyCode.USD): IO[Contact] =
    ok(books.catalog.createContact(CatalogService.NewContact(kind, Title.applyUnsafe(name), None, Some(currency))))

  private def stockItem(sku: String, sell: Long, cost: Long): IO[Item] =
    ok(books.catalog.createItem(CatalogService.NewItem(Sku.applyUnsafe(sku), Title.applyUnsafe(sku), ItemKind.Inventory,
                                                       Some(p(sell)), Some(p(cost)), None, None, None)))

  private def serviceItem(sku: String, sell: Long): IO[Item] =
    ok(books.catalog.createItem(CatalogService.NewItem(Sku.applyUnsafe(sku), Title.applyUnsafe(sku), ItemKind.Service,
                                                       Some(p(sell)), None, None, None, None)))

  private def taxRate(code: String, bps: Int): IO[TaxRate] =
    ok(books.catalog.createTaxRate(CatalogService.NewTaxRate(TaxCode.applyUnsafe(code), Title.applyUnsafe(code), Bps.assume(bps))))

  private def itemLine(i: Item, qty: String, price: Option[Long] = None, tax: Option[TaxRate] = None) =
    NewLine(Some(i.id), None, q(qty), price.map(p), None, tax.map(_.id))

  private def doc(kind: DocumentKind, c: Contact, date: LocalDate, lines: NewLine*): IO[Either[LedgerError, DocumentDetails]] =
    books.trade.createDocument(kind, NewDocument(c.id, date, None, None, None, None, lines.toList), accountant)

  private def settle(kind: DocumentKind, c: Contact, date: LocalDate, rate: Option[String], allocs: (DocumentDetails, Long)*) =
    books.trade.settle(kind, NewSettlement(c.id, date, SystemAccounts.Cash.id, None, None,
                                           rate.map(r => ExchangeRate.assume(BigDecimal(r))),
                                           allocs.toList.map { case (dd, amt) => NewAllocation(dd.document.id, p(amt)) }), accountant)

  private def stockOf(i: Item): IO[(BigDecimal, Long)] =
    books.trade.inventoryValuation(farFuture).map(_.rows.find(_.itemId == i.id).fold((BigDecimal(0), 0L))(r => (r.quantity, r.valueMinor.value)))

  /** Base-currency balance of the receivable/payable lines tagged with `c`. */
  private def controlBalance(account: AccountId, c: Contact): IO[Long] =
    scalar(s"""SELECT COALESCE(SUM(CASE WHEN side = 'debit' THEN base_amount_minor ELSE -base_amount_minor END), 0)::int8
               FROM journal_lines WHERE account_id = '${account.value}' AND contact_id = '${c.id.value}'""")

  private def status(dd: DocumentDetails): IO[DocumentStatus] =
    books.trade.document(dd.document.id).map(_.get.status)

  test("bills receive stock at cost; invoices issue it at moving average and post cost of goods sold") {
    for {
      vendor   <- contact(ContactKind.Vendor, "Widget Works")
      customer <- contact(ContactKind.Customer, "Acme")
      widget   <- stockItem("W-1", sell = 2_500, cost = 1_000)
      vat      <- taxRate("VAT20", 2_000)
      bill1    <- ok(doc(DocumentKind.Bill, vendor, d(1, 10), itemLine(widget, "10", tax = Some(vat))))
      _        <- ok(doc(DocumentKind.Bill, vendor, d(1, 11), itemLine(widget, "10", price = Some(1_300))))
      inv      <- ok(doc(DocumentKind.Invoice, customer, d(1, 20), itemLine(widget, "5", tax = Some(vat))))
      stock    <- stockOf(widget)
      _        <- failsWith(doc(DocumentKind.Invoice, customer, d(1, 21), itemLine(widget, "100"))) {
                    case LedgerError.InsufficientStock(_, onHand, _) if onHand == BigDecimal(15) =>
                  }
      next     <- ok(doc(DocumentKind.Invoice, customer, d(1, 22), itemLine(widget, "1")))
    } yield {
      assertEquals(bill1.document.totalMinor.value: Long, 12_000L)
      assertEquals(bill1.document.number, "BILL-000001")
      assertEquals(inv.document.totalMinor.value: Long, 15_000L)
      assertEquals(stock, (BigDecimal(15), 23_000L - 5_750L))   // 5 of 20 at 23,000
      assertEquals(next.document.number, "INV-000002")             // the rejected invoice consumed no number
    }
  }

  test("receipts settle invoices: partial, over-allocation, voids and status") {
    for {
      c     <- contact(ContactKind.Customer, "Beta")
      svc   <- serviceItem("SETUP-1", 10_000)
      inv   <- ok(doc(DocumentKind.Invoice, c, d(2, 1), itemLine(svc, "1")))
      r1    <- ok(settle(DocumentKind.Invoice, c, d(2, 5), None, inv -> 4_000))
      s1    <- status(inv)
      _     <- failsWith(settle(DocumentKind.Invoice, c, d(2, 6), None, inv -> 7_000)) { case LedgerError.OverAllocation(_, 6_000L, 7_000L) => }
      r2    <- ok(settle(DocumentKind.Invoice, c, d(2, 7), None, inv -> 6_000))
      s2    <- status(inv)
      _     <- failsWith(books.trade.voidDocument(inv.document.id, d(2, 8), accountant)) { case LedgerError.DocumentHasPayments => }
      _     <- ok(books.trade.voidSettlement(r2.settlement.id, d(2, 9), accountant))
      _     <- failsWith(books.trade.voidSettlement(r2.settlement.id, d(2, 9), accountant)) { case LedgerError.AlreadyVoided(_) => }
      s3    <- status(inv)
      _     <- ok(books.trade.voidSettlement(r1.settlement.id, d(2, 9), accountant))
      voided <- ok(books.trade.voidDocument(inv.document.id, d(2, 10), accountant))
      ar    <- controlBalance(SystemAccounts.Receivables.id, c)
    } yield {
      assertEquals(r1.settlement.number.take(5), "RCPT-")
      assertEquals(List(s1, s2, s3), List(DocumentStatus.PartiallyPaid, DocumentStatus.Paid, DocumentStatus.PartiallyPaid))
      assertEquals(voided.status, DocumentStatus.Void)
      assertEquals(ar, 0L)
    }
  }

  test("foreign-currency receipts realise exchange differences and clear the receivable exactly") {
    for {
      c    <- contact(ContactKind.Customer, "Gamma GmbH", CurrencyCode.EUR)
      svc  <- serviceItem("AUDIT-1", 99_999)
      _    <- books.ledger.setRate(FxRate(CurrencyCode.EUR, d(3, 1), ExchangeRate.assume(BigDecimal("1.0833"))))
      inv  <- ok(doc(DocumentKind.Invoice, c, d(3, 2), itemLine(svc, "1")))
      a    <- ok(settle(DocumentKind.Invoice, c, d(3, 10), Some("1.2"), inv -> 33_333))
      b    <- ok(settle(DocumentKind.Invoice, c, d(3, 11), Some("0.9777"), inv -> 33_333))
      last <- ok(settle(DocumentKind.Invoice, c, d(3, 12), Some("1.0833"), inv -> 33_333))
      ar   <- controlBalance(SystemAccounts.Receivables.id, c)
      st   <- status(inv)
      aged <- books.trade.aging(DocumentKind.Invoice, farFuture)
    } yield {
      assertEquals(inv.document.currency, CurrencyCode.EUR)
      assertEquals(inv.document.controlBaseMinor.value: Long, 108_329L)   // round(99,999 x 1.0833)
      assert(a.settlement.fxEntryId.isDefined, "gain expected at 1.2")
      assert(b.settlement.fxEntryId.isDefined, "loss expected at 0.9777")
      assertEquals(ar, 0L)
      assertEquals(st, DocumentStatus.Paid)
      assert(!aged.documents.exists(_.documentId == inv.document.id))
      assertEquals(last.allocations.map(_.amountMinor.value: Long), List(33_333L))
    }
  }

  test("voiding an invoice returns its stock; voiding a bill needs that stock on hand") {
    for {
      vendor   <- contact(ContactKind.Vendor, "Parts Inc")
      customer <- contact(ContactKind.Customer, "Delta")
      part     <- stockItem("P-1", sell = 900, cost = 500)
      bill     <- ok(doc(DocumentKind.Bill, vendor, d(4, 1), itemLine(part, "4")))
      inv      <- ok(doc(DocumentKind.Invoice, customer, d(4, 2), itemLine(part, "3")))
      afterSale <- stockOf(part)
      _        <- failsWith(books.trade.voidDocument(bill.document.id, d(4, 3), accountant)) { case LedgerError.StockAlreadyConsumed(_, _, _) => }
      _        <- ok(books.trade.voidDocument(inv.document.id, d(4, 3), accountant))
      restored <- stockOf(part)
      _        <- ok(books.trade.voidDocument(bill.document.id, d(4, 4), accountant))
      emptied  <- stockOf(part)
      _        <- failsWith(books.trade.voidDocument(bill.document.id, d(4, 5), accountant)) { case LedgerError.AlreadyVoided(_) => }
    } yield {
      assertEquals(afterSale, (BigDecimal(1), 500L))
      assertEquals(restored, (BigDecimal(4), 2_000L))
      assertEquals(emptied, (BigDecimal(0), 0L))
    }
  }

  test("stock adjustments value increases at cost and decreases at average") {
    val adj = (i: Item, qty: String, value: Option[Long]) =>
      books.trade.adjustStock(TradeService.NewAdjustment(i.id, d(5, 1), BigDecimal(qty), value.map(p), None, Body.applyUnsafe("count")), accountant)
    for {
      item <- stockItem("A-1", sell = 1_000, cost = 700)
      up   <- ok(adj(item, "5", None))
      up2  <- ok(adj(item, "5", Some(4_000)))
      down <- ok(adj(item, "-2", None))
      _    <- failsWith(adj(item, "-100", None)) { case LedgerError.InsufficientStock(_, _, _) => }
      svc  <- serviceItem("S-ADJ", 100)
      _    <- failsWith(adj(svc, "1", Some(1))) { case LedgerError.NotAnInventoryItem(_) => }
      left <- stockOf(item)
    } yield {
      assertEquals(up.valueMinor, 3_500L)
      assertEquals(up2.valueMinor, 4_000L)
      assertEquals(down.valueMinor, -1_500L)   // 2 of 10 at 7,500
      assertEquals(left, (BigDecimal(8), 6_000L))
    }
  }

  test("aging buckets a customer's open invoices by days past due") {
    for {
      c   <- contact(ContactKind.Customer, "Epsilon")
      svc <- serviceItem("RETAINER", 1_000)
      mk   = (date: LocalDate, due: LocalDate) =>
               ok(books.trade.createDocument(DocumentKind.Invoice,
                    NewDocument(c.id, date, Some(due), None, None, None, List(itemLine(svc, "1"))), accountant))
      _   <- mk(d(6, 1), d(6, 30))    // current at 6/30
      _   <- mk(d(5, 1), d(5, 31))    // 30 days
      _   <- mk(d(3, 1), d(3, 31))    // 91 days
      rep <- books.trade.aging(DocumentKind.Invoice, d(6, 30))
    } yield {
      val row = rep.rows.find(_.contactId == c.id).get.buckets
      assertEquals(List(row.currentMinor, row.days1To30Minor, row.days31To60Minor, row.days61To90Minor, row.over90Minor)
                     .map(a => a.value: Long), List(1_000L, 1_000L, 0L, 0L, 1_000L))
    }
  }

  test("sales tax report nets tax charged against tax recoverable") {
    for {
      gst      <- taxRate("GST10", 1_000)
      customer <- contact(ContactKind.Customer, "Zeta")
      vendor   <- contact(ContactKind.Vendor, "Eta Supplies")
      svc      <- serviceItem("TAXED-1", 1_000)
      _        <- ok(doc(DocumentKind.Invoice, customer, d(9, 1), itemLine(svc, "1", tax = Some(gst))))
      _        <- ok(doc(DocumentKind.Bill, vendor, d(9, 2), NewLine(None, None, q("1"), Some(p(400)), Some(SystemAccounts.GeneralExpenses.id), Some(gst.id))))
      rep      <- ok(books.trade.salesTax(d(9, 1), d(9, 30)))
    } yield {
      val row = rep.rows.find(_.taxRateId == gst.id).get
      assertEquals(row.taxCollectedMinor.value: Long, 100L)
      assertEquals(row.taxPaidMinor.value: Long, 40L)
      assertEquals(row.netMinor.value: Long, 60L)
    }
  }

  test("bill payments settle bills and the sub-ledgers reconcile to the general ledger") {
    for {
      vendor <- contact(ContactKind.Vendor, "Theta")
      bill   <- ok(doc(DocumentKind.Bill, vendor, d(10, 1), NewLine(None, None, q("1"), Some(p(2_000)), Some(SystemAccounts.GeneralExpenses.id), None)))
      pay    <- ok(settle(DocumentKind.Bill, vendor, d(10, 5), None, bill -> 1_500))
      tb     <- books.ledger.trialBalance(farFuture)
      bs     <- books.ledger.balanceSheet(farFuture)
      ar     <- books.trade.aging(DocumentKind.Invoice, farFuture)
      ap     <- books.trade.aging(DocumentKind.Bill, farFuture)
      inv    <- books.trade.inventoryValuation(farFuture)
    } yield {
      def gl(id: AccountId) = tb.rows.find(_.accountId == id).fold(0L)(r => (r.debitMinor.value: Long) - r.creditMinor.value)
      assertEquals(pay.settlement.number, "PAY-000001")
      assertEquals(tb.totalDebitMinor.value: Long, tb.totalCreditMinor.value: Long)
      assertEquals(bs.totalAssetsMinor.value: Long, bs.totalLiabilitiesAndEquityMinor.value: Long)
      assertEquals(gl(SystemAccounts.Inventory.id), inv.totalValueMinor.value: Long)
      assertEquals(gl(SystemAccounts.Receivables.id), ar.totals.totalMinor.value: Long)
      assertEquals(-gl(SystemAccounts.Payables.id), ap.totals.totalMinor.value: Long)
    }
  }
}
