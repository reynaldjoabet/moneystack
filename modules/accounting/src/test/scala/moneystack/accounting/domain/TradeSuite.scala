package moneystack.accounting.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

class TradeSuite extends munit.FunSuite {

  private def id[A](f: UUID => A): A = f(UUID.randomUUID())

  private val customer = id(ContactId.assume)
  private val ar       = id(AccountId.assume)
  private val tax      = id(AccountId.assume)
  private val sales    = id(AccountId.assume)
  private val services = id(AccountId.assume)
  private val vat20    = id(TaxRateId.assume)
  private val vat5     = id(TaxRateId.assume)

  private def line(no: Int, account: AccountId, amount: Long, taxRate: Option[(TaxRateId, Int)]): DocumentLine =
    DocumentLine(no, None, None, Quantity.assume(BigDecimal(1)), PriceMinor.assume(amount.max(1L)), account,
                 taxRate.map(_._1), taxRate.map(r => Bps.assume(r._2)),
                 AmountMinor.assume(amount), AmountMinor.assume(Trade.lineTax(amount, taxRate.map(r => Bps.assume(r._2)))))

  private def total(lines: List[JournalLine], side: Side): Long =
    lines.filter(_.side == side).map(_.amountMinor.value: Long).sum

  test("line amounts and tax round half-even") {
    assertEquals(Trade.lineAmount(BigDecimal("2.5"), 3), 8L)     // 7.5 -> 8
    assertEquals(Trade.lineAmount(BigDecimal("0.5"), 5), 2L)     // 2.5 -> 2
    assertEquals(Trade.lineTax(1_005, Some(Bps.assume(1_000))), 100L)  // 100.5 -> 100
    assertEquals(Trade.lineTax(1_015, Some(Bps.assume(1_000))), 102L)  // 101.5 -> 102
    assertEquals(Trade.lineTax(1_000, None), 0L)
  }

  test("invoice posts receivable against income and grouped tax, balanced") {
    val lines = List(line(1, sales, 10_000, Some(vat20 -> 2_000)), line(2, services, 5_000, Some(vat20 -> 2_000)),
                     line(3, sales, 2_000, Some(vat5 -> 500)), line(4, sales, 0, None))
    val posted = Trade.postingLines(DocumentKind.Invoice, customer, lines, ar, tax)

    assertEquals(posted.head, JournalLine(ar, Side.Debit, PriceMinor.assume(17_000L + 3_000L + 100L), Some(customer)))
    assertEquals(posted.count(_.accountId == tax), 2)                      // one per rate
    assertEquals(posted.filter(_.accountId == tax).map(_.amountMinor.value: Long), List(3_000L, 100L))
    assertEquals(posted.size, 1 + 3 + 2)                                   // zero line dropped
    assertEquals(total(posted, Side.Debit), total(posted, Side.Credit))
  }

  test("bill mirrors the invoice sides") {
    val posted = Trade.postingLines(DocumentKind.Bill, customer, List(line(1, services, 4_000, Some(vat20 -> 2_000))), ar, tax)
    assertEquals(posted.map(_.side), List(Side.Credit, Side.Debit, Side.Debit))
    assertEquals(total(posted, Side.Debit), total(posted, Side.Credit))
  }

  test("line bases come back from the posted entry in line order, skipping zero lines") {
    val lines  = List(line(1, sales, 300, None), line(2, sales, 0, None), line(3, sales, 700, None))
    val jl     = Trade.postingLines(DocumentKind.Bill, customer, lines, ar, tax)
    val posted = Ledger.toBase(jl, ExchangeRate.assume(BigDecimal("1.5")))
    assertEquals(Trade.lineBases(posted, lines), Map(1 -> 450L, 3 -> 1_050L))
  }

  test("the allocation completing payment clears the booked base exactly") {
    val doc = Document(id(DocumentId.assume), DocumentKind.Invoice, "INV-000001", customer, LocalDate.of(2026, 1, 1),
                       LocalDate.of(2026, 1, 31), CurrencyCode.EUR, ExchangeRate.assume(BigDecimal("1.0833")), None, None,
                       AmountMinor.assume(1_000), AmountMinor.assume(0), PriceMinor.assume(1_000), AmountMinor.assume(1_083),
                       id(JournalEntryId.assume), None, None, Instant.EPOCH)
    val first  = Trade.documentBase(doc, 0L, 0L, 333L)          // round(333 * 1.0833) = 361
    val second = Trade.documentBase(doc, 333L, first, 333L)     // 361
    val last   = Trade.documentBase(doc, 666L, first + second, 334L)
    assertEquals(first + second + last, 1_083L)
  }

  test("fx difference: receipts gain on a stronger rate, payments lose") {
    val fx = id(AccountId.assume)
    val gain = Trade.fxLines(DocumentKind.Invoice, customer, 25L, ar, fx)
    assertEquals(gain.map(l => (l.accountId, l.side)), List(ar -> Side.Debit, fx -> Side.Credit))
    val loss = Trade.fxLines(DocumentKind.Bill, customer, 25L, ar, fx)
    assertEquals(loss.map(l => (l.accountId, l.side)), List(ar -> Side.Credit, fx -> Side.Debit))
    val negative = Trade.fxLines(DocumentKind.Invoice, customer, -25L, ar, fx)
    assertEquals(negative.map(l => (l.accountId, l.side)), List(ar -> Side.Credit, fx -> Side.Debit))
    assert(Trade.fxLines(DocumentKind.Invoice, customer, 0L, ar, fx).isEmpty)
  }

  test("moving average issue takes the remainder exactly when emptying stock") {
    val stock = Stock(id(ItemId.assume), BigDecimal(3), 1_000L)
    assertEquals(stock.costOf(BigDecimal(1)), 333L)
    val after = stock.add(BigDecimal(-1), -333L)
    assertEquals(after.costOf(BigDecimal(2)), 667L)
  }

  test("status follows payments and voids") {
    val doc = Document(id(DocumentId.assume), DocumentKind.Bill, "BILL-000001", customer, LocalDate.of(2026, 1, 1),
                       LocalDate.of(2026, 1, 31), CurrencyCode.USD, ExchangeRate.One, None, None, AmountMinor.assume(500),
                       AmountMinor.assume(0), PriceMinor.assume(500), AmountMinor.assume(500), id(JournalEntryId.assume),
                       None, None, Instant.EPOCH)
    assertEquals(Trade.status(doc, 0L), DocumentStatus.Open)
    assertEquals(Trade.status(doc, 200L), DocumentStatus.PartiallyPaid)
    assertEquals(Trade.status(doc, 500L), DocumentStatus.Paid)
    assertEquals(Trade.status(doc.copy(voidedOn = Some(LocalDate.of(2026, 1, 2))), 0L), DocumentStatus.Void)
  }
}
