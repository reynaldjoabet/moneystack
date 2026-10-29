package moneystack.accounting.it

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.service.{CashService, CatalogService, LedgerService, TradeService, WorkflowService}
import moneystack.accounting.service.LedgerService.SystemAccounts
import moneystack.accounting.service.TradeService.{NewDocument, NewLine}

import cats.effect.*
import cats.syntax.all.*

import java.time.LocalDate

class WorkflowIntegrationSuite extends PostgresSuite {

  private def d(m: Int, day: Int) = LocalDate.of(2026, m, day)
  private def t(s: String)        = Title.applyUnsafe(s)

  private def customer(name: String) =
    ok(books.catalog.createContact(CatalogService.NewContact(ContactKind.Customer, t(name), None, Some(CurrencyCode.USD))))

  private def consulting(c: Contact, date: LocalDate, due: Option[LocalDate] = None, amount: Long = 50_000) =
    NewDocument(
      c.id,
      date,
      due,
      None,
      None,
      None,
      List(NewLine(
        None,
        Some(Body.applyUnsafe("Consulting")),
        Quantity.assume(BigDecimal(1)),
        Some(PriceMinor.assume(amount)),
        Some(SystemAccounts.Sales.id),
        None
      ))
    )

  private def events(id: DocumentId) = ok(books.workflow.history(id)).map(_.map(_.kind))

  test("drafts post nothing until issued; issuing numbers the document and starts its history") {
    for {
      acme   <- customer("Acme Drafts")
      vendor <- ok(books.catalog.createContact(CatalogService.NewContact(ContactKind.Vendor, t("Wrong side"), None, None)))
      before <- books.ledger.trialBalance(d(12, 31))
      draft  <- ok(books.workflow.saveDraft(DocumentKind.Invoice, None, consulting(acme, d(1, 5)), accountant))
      edited <-
        ok(books.workflow.saveDraft(DocumentKind.Invoice, Some(draft.draft.id), consulting(acme, d(1, 6), amount = 70_000), accountant))
      _ <- failsWith(books.workflow.saveDraft(DocumentKind.Invoice, None, consulting(vendor, d(1, 6)), accountant)) {
        case LedgerError.ContactKindMismatch(_, _) =>
      }
      list   <- books.workflow.drafts(DocumentKind.Invoice)
      after  <- books.ledger.trialBalance(d(12, 31))
      issued <- ok(books.workflow.issueDraft(draft.draft.id, accountant))
      _      <- failsWith(books.workflow.issueDraft(draft.draft.id, accountant)) { case LedgerError.DraftNotFound(_) => }
      _      <- ok(books.workflow.markSent(issued.document.id, Some(Body.applyUnsafe("emailed to ap@acme")), accountant))
      hist   <- events(issued.document.id)
      left   <- books.workflow.drafts(DocumentKind.Invoice)
    } yield {
      assertEquals(edited.draft.id, draft.draft.id)
      assertEquals(list.map(_.document.lines.head.unitPriceMinor.map(p => p.value: Long)), List(Some(70_000L)))
      assertEquals(after.totalDebitMinor, before.totalDebitMinor) // a draft posts nothing
      assertEquals(issued.document.totalMinor.value: Long, 70_000L)
      assert(issued.document.number.startsWith("INV-"))
      assertEquals(hist, List(DocumentEventKind.Issued, DocumentEventKind.Sent))
      assertEquals(left, Nil)
    }
  }

  test("recurring invoices issue each due occurrence once, keeping payment terms") {
    for {
      club <- customer("Monthly Retainer Ltd")
      rec  <- ok(books.workflow.createRecurring(
        DocumentKind.Invoice,
        WorkflowService.NewRecurring(Every.Month, None, None, Some(4), consulting(club, d(1, 31), Some(d(2, 14)), 20_000)),
        accountant
      ))
      first <- books.workflow.runRecurring(d(3, 31), accountant)
      again <- books.workflow.runRecurring(d(3, 31), accountant)
      later <- books.workflow.runRecurring(d(12, 31), accountant)
      docs  <- first.issued.traverse(id => books.trade.document(id).map(_.get.document))
      view  <- books.workflow.recurring.map(_.find(_.recurring.id == rec.recurring.id).get)
      pay   <- ok(books.trade.settle(
        DocumentKind.Invoice,
        TradeService.NewSettlement(
          club.id,
          d(2, 20),
          SystemAccounts.Cash.id,
          None,
          None,
          None,
          List(TradeService.NewAllocation(first.issued.head, PriceMinor.assume(20_000)))
        ),
        accountant
      ))
      _     <- ok(books.trade.voidDocument(first.issued(1), d(3, 1), accountant))
      paidH <- events(first.issued.head)
      voidH <- events(first.issued(1))
    } yield {
      assertEquals(docs.map(x => (x.date, x.dueDate)), List(d(1, 31) -> d(2, 14), d(2, 28) -> d(3, 14), d(3, 31) -> d(4, 14)))
      assertEquals(again.issued, Nil)
      assertEquals(later.issued.size, 1) // max four occurrences
      assertEquals(view.nextDue, None)
      assertEquals(pay.allocations.size, 1)
      assertEquals(paidH, List(DocumentEventKind.Issued, DocumentEventKind.Payment))
      assertEquals(voidH, List(DocumentEventKind.Issued, DocumentEventKind.Voided))
    }
  }

  test("direct income, expenses and transfers post balanced entries between the right accounts") {
    for {
      in <- ok(books.cash.moneyIn(
        CashService.NewMovement(
          SystemAccounts.Cash.id,
          SystemAccounts.Sales.id,
          PriceMinor.assume(9_000),
          d(4, 1),
          None,
          None,
          None,
          None,
          Some(Body.applyUnsafe("cash sale"))
        ),
        accountant
      ))
      out <- ok(books.cash.moneyOut(
        CashService.NewMovement(
          SystemAccounts.Cash.id,
          SystemAccounts.GeneralExpenses.id,
          PriceMinor.assume(2_500),
          d(4, 2),
          None,
          None,
          None,
          None,
          None
        ),
        accountant
      ))
      tr <- ok(books.cash.transfer(
        CashService.NewTransfer(
          SystemAccounts.Cash.id,
          SystemAccounts.PaymentClearing.id,
          PriceMinor.assume(1_000),
          d(4, 3),
          None,
          None,
          None
        ),
        accountant
      ))
      _ <- failsWith(books.cash.moneyIn(
        CashService.NewMovement(
          SystemAccounts.Cash.id,
          SystemAccounts.PaymentClearing.id,
          PriceMinor.assume(1),
          d(4, 4),
          None,
          None,
          None,
          None,
          None
        ),
        accountant
      )) { case LedgerError.WrongAccountType(_, _) => }
      _ <- failsWith(books.cash.transfer(
        CashService.NewTransfer(
          SystemAccounts.Cash.id,
          SystemAccounts.Cash.id,
          PriceMinor.assume(1),
          d(4, 4),
          None,
          None,
          None
        ),
        accountant
      )) { case LedgerError.InvalidDraft(_) => }
      tb <- books.ledger.trialBalance(d(12, 31))
    } yield {
      assertEquals(in.header.source, JournalSource.MoneyIn)
      assertEquals(
        out.lines.map(l => (l.accountId, l.side)),
        List(SystemAccounts.GeneralExpenses.id -> Side.Debit, SystemAccounts.Cash.id -> Side.Credit)
      )
      assertEquals(tr.header.source, JournalSource.Transfer)
      assertEquals(tb.totalDebitMinor.value: Long, tb.totalCreditMinor.value: Long)
    }
  }

  test("reconciliation ticks lines off against the statement, then carries the balance forward") {
    for {
      bank <- ok(books.ledger.createAccount(LedgerService.NewAccount(
        AccountCode.applyUnsafe("1050"),
        t("Reconciled bank"),
        AccountType.Bank,
        None,
        None
      )))
      mv = (amount: Long, date: LocalDate, inbound: Boolean) => {
        val m = CashService.NewMovement(
          bank.id,
          if (inbound) SystemAccounts.Sales.id else SystemAccounts.GeneralExpenses.id,
          PriceMinor.assume(amount),
          date,
          None,
          None,
          None,
          None,
          None
        )
        ok(if (inbound) books.cash.moneyIn(m, accountant) else books.cash.moneyOut(m, accountant))
      }
      e1  <- mv(50_000, d(5, 5), true)
      e2  <- mv(12_000, d(5, 10), false)
      e3  <- mv(3_000, d(6, 2), true)
      jan <- ok(books.cash.startReconciliation(CashService.NewReconciliation(bank.id, d(5, 1), d(5, 31), 38_000), accountant))
      _   <- failsWith(books.cash.startReconciliation(CashService.NewReconciliation(bank.id, d(6, 1), d(6, 30), 0), accountant)) {
        case LedgerError.InvalidDraft(_) =>
      }
      part <- ok(books.cash.setCleared(jan.reconciliation.id, List(e1.header.id -> 1), cleared = true))
      _    <- failsWith(books.cash.closeReconciliation(jan.reconciliation.id)) { case LedgerError.ReconciliationUnbalanced(-12_000L) => }
      _    <- failsWith(books.cash.setCleared(jan.reconciliation.id, List(e3.header.id -> 1), cleared = true)) {
        case LedgerError.LineNotOnAccount(_, _) =>
      }
      _    <- ok(books.cash.setCleared(jan.reconciliation.id, List(e2.header.id -> 2), cleared = true))
      done <- ok(books.cash.closeReconciliation(jan.reconciliation.id))
      _    <- failsWith(books.cash.setCleared(jan.reconciliation.id, List(e2.header.id -> 2), cleared = false)) {
        case LedgerError.ReconciliationClosed =>
      }
      _ <- failsWith(books.cash.startReconciliation(CashService.NewReconciliation(bank.id, d(5, 20), d(6, 30), 0), accountant)) {
        case LedgerError.InvalidPeriod(_, _) =>
      }
      feb <- ok(books.cash.startReconciliation(CashService.NewReconciliation(bank.id, d(6, 1), d(6, 30), 41_000), accountant))
      fin <- ok(books.cash.setCleared(feb.reconciliation.id, List(e3.header.id -> 1), cleared = true))
    } yield {
      assertEquals(jan.lines.map(_.amountMinor), List(50_000L, -12_000L)) // e3 is after the period
      assertEquals((part.clearedMinor, part.differenceMinor), (50_000L, -12_000L))
      assertEquals(done.reconciliation.status, ReconciliationStatus.Closed)
      assertEquals(feb.reconciliation.openingMinor, 38_000L)
      assertEquals(feb.lines.map(_.entryId), List(e3.header.id)) // last period's lines are settled
      assertEquals(fin.differenceMinor, 0L)
    }
  }
}
