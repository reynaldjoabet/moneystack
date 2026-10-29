package wealth.accounting.it

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.service.LedgerService
import wealth.accounting.service.LedgerService.SystemAccounts


import java.time.{Instant, LocalDate}
import java.util.UUID

class LedgerIntegrationSuite extends PostgresSuite {

  private def d(m: Int, day: Int) = LocalDate.of(2026, m, day)
  private def amt(l: Long)        = PriceMinor.assume(l)
  private def dr(a: LedgerService.SystemAccount, l: Long) = JournalLine(a.id, Side.Debit, amt(l), None)
  private def cr(a: LedgerService.SystemAccount, l: Long) = JournalLine(a.id, Side.Credit, amt(l), None)

  private def draft(date: LocalDate, lines: JournalLine*) =
    LedgerService.Draft(date, None, None, CurrencyCode.USD, None, lines.toList)

  test("bootstrap creates the system chart and is idempotent") {
    for {
      _     <- books.bootstrap
      accts <- books.ledger.accounts
    } yield {
      assertEquals(accts.size, SystemAccounts.all.size)
      assertEquals(accts.map(_.id).toSet, SystemAccounts.all.map(_.id).toSet)
    }
  }

  test("posts, reverses once, and keeps the trial balance balanced") {
    for {
      e   <- ok(books.ledger.post(draft(d(1, 5), dr(SystemAccounts.Cash, 50_000), cr(SystemAccounts.OwnersEquity, 50_000)), accountant))
      rev <- ok(books.ledger.reverse(e.header.id, d(1, 6), accountant))
      _   <- failsWith(books.ledger.reverse(e.header.id, d(1, 7), accountant)) { case LedgerError.AlreadyReversed(_, r) if r == rev.header.id => }
      tb  <- books.ledger.trialBalance(d(12, 31))
    } yield {
      assertEquals(rev.lines.map(_.side), e.lines.map(_.side.opposite))
      assertEquals(tb.totalDebitMinor.value: Long, tb.totalCreditMinor.value: Long)
      assert(!tb.rows.exists(_.accountId == SystemAccounts.OwnersEquity.id))   // nets to zero
    }
  }

  test("foreign-currency entries use the stored rate on or before the entry date") {
    for {
      _   <- books.ledger.setRate(FxRate(CurrencyCode.EUR, d(2, 1), ExchangeRate.assume(BigDecimal("1.1"))))
      _   <- books.ledger.setRate(FxRate(CurrencyCode.EUR, d(2, 10), ExchangeRate.assume(BigDecimal("1.2"))))
      e   <- ok(books.ledger.post(draft(d(2, 9), dr(SystemAccounts.Cash, 1_000), cr(SystemAccounts.OwnersEquity, 1_000))
                                    .copy(currency = CurrencyCode.EUR), accountant))
      _   <- failsWith(books.ledger.post(draft(d(1, 31), dr(SystemAccounts.Cash, 1), cr(SystemAccounts.OwnersEquity, 1))
                                           .copy(currency = CurrencyCode.EUR), accountant)) { case LedgerError.MissingExchangeRate(_, _) => }
    } yield {
      assertEquals(e.header.exchangeRate.value, BigDecimal("1.1"))
      assertEquals(e.lines.map(_.baseAmountMinor.value: Long), List(1_100L, 1_100L))
    }
  }

  test("automated postings are idempotent per source") {
    val source = UUID.randomUUID()
    val pay    = draft(d(3, 1), dr(SystemAccounts.PaymentClearing, 999), cr(SystemAccounts.SubscriptionRevenue, 999))
    for {
      first  <- ok(books.ledger.postAutomated(pay, JournalSource.Payment, source))
      second <- ok(books.ledger.postAutomated(pay, JournalSource.Payment, source))
    } yield assertEquals(second.header.id, first.header.id)
  }

  test("the database rejects edits, deletes and unbalanced entries") {
    for {
      upd <- rejectedSql("UPDATE journal_lines SET amount_minor = amount_minor + 1")
      del <- rejectedSql("DELETE FROM journal_entries")
      unb <- rejectedSql(   // a header without lines; the deferred trigger fires at commit
               s"""INSERT INTO journal_entries (id, entry_date, currency, exchange_rate, source)
                   VALUES ('${UUID.randomUUID()}', '2026-04-01', 'USD', 1, 'manual')"""
             )
    } yield {
      assert(upd.contains("append-only"), upd)
      assert(del.contains("append-only"), del)
      assert(unb.contains("unbalanced"), unb)
    }
  }

  test("period lock blocks postings except inside the reopened window") {
    val lock = LedgerLock(d(5, 31), Some(d(5, 1)), Some(d(5, 31)), Body.applyUnsafe("April close"), accountant, Instant.now())
    for {
      _ <- books.ledger.setLock(lock)
      _ <- failsWith(books.ledger.post(draft(d(4, 15), dr(SystemAccounts.Cash, 1), cr(SystemAccounts.OwnersEquity, 1)), accountant)) {
             case LedgerError.PeriodLocked(_, through) if through == d(5, 31) =>
           }
      _ <- ok(books.ledger.post(draft(d(5, 15), dr(SystemAccounts.Cash, 1), cr(SystemAccounts.OwnersEquity, 1)), accountant))
      _ <- books.ledger.clearLock
      _ <- ok(books.ledger.post(draft(d(4, 15), dr(SystemAccounts.Cash, 1), cr(SystemAccounts.OwnersEquity, 1)), accountant))
    } yield ()
  }

  test("statements: balance sheet balances, general ledger runs, P&L reflects revenue") {
    for {
      _  <- ok(books.ledger.post(draft(d(6, 1), dr(SystemAccounts.Cash, 7_000), cr(SystemAccounts.Sales, 7_000)), accountant))
      _  <- ok(books.ledger.post(draft(d(6, 2), dr(SystemAccounts.GeneralExpenses, 2_500), cr(SystemAccounts.Cash, 2_500)), accountant))
      bs <- books.ledger.balanceSheet(d(12, 31))
      pl <- ok(books.ledger.profitAndLoss(d(6, 1), d(6, 30)))
      gl <- ok(books.ledger.generalLedger(d(6, 1), d(6, 30), Some(SystemAccounts.Cash.id)))
    } yield {
      assertEquals(bs.totalAssetsMinor.value: Long, bs.totalLiabilitiesAndEquityMinor.value: Long)
      assertEquals(pl.netIncomeMinor.value: Long, 4_500L)
      val cash = gl.accounts.head
      assertEquals(cash.lines.map(_.balanceMinor.value: Long), List(cash.openingMinor.value + 7_000L, cash.openingMinor.value + 4_500L))
    }
  }
}
