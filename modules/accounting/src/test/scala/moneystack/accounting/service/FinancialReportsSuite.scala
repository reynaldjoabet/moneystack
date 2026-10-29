package moneystack.accounting.service

import moneystack.domain.*
import moneystack.accounting.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

class FinancialReportsSuite extends munit.FunSuite {

  private val usd = CurrencyCode.USD
  private val jan = LocalDate.of(2026, 1, 1)
  private val dec = LocalDate.of(2026, 12, 31)

  private def account(code: String, t: AccountType): Account =
    Account(AccountId.assume(UUID.randomUUID()), AccountCode.applyUnsafe(code), Title.applyUnsafe(s"acct $code"),
            t, usd, None, active = true, Instant.EPOCH)

  private val cash     = account("1000", AccountType.Cash)
  private val ar       = account("1100", AccountType.AccountsReceivable)
  private val laptop   = account("1500", AccountType.FixedAsset)
  private val payable  = account("2000", AccountType.AccountsPayable)
  private val loan     = account("2500", AccountType.LongTermLiability)
  private val capital  = account("3000", AccountType.Equity)
  private val revenue  = account("4000", AccountType.Income)
  private val interest = account("4900", AccountType.OtherIncome)
  private val fees     = account("5000", AccountType.CostOfSales)
  private val rent     = account("6000", AccountType.Expense)
  private val fxLoss   = account("7000", AccountType.OtherExpense)
  private val unused   = account("6999", AccountType.Expense)

  private val chart = List(rent, cash, ar, laptop, payable, loan, capital, revenue, interest, fees, fxLoss, unused)

  /** A small, balanced year: (debit account, credit account, amount). */
  private val postings = List(
    (cash, capital, 50_000L),    // owner invests
    (cash, loan, 20_000L),       // borrow
    (laptop, payable, 3_000L),   // buy equipment on credit
    (cash, revenue, 12_000L),    // cash sales
    (ar, revenue, 4_000L),       // sales on account
    (fees, cash, 600L),          // processing fees
    (rent, cash, 9_000L),        // rent
    (cash, interest, 150L),      // interest earned
    (fxLoss, cash, 50L),         // fx loss
    (payable, cash, 1_000L)      // pay down supplier
  )

  private val totals: List[AccountTotals] = {
    val legs = postings.flatMap { case (dr, cr, amt) => List((dr.id, amt, 0L), (cr.id, 0L, amt)) }
    legs.groupMapReduce(_._1)(l => (l._2, l._3))((a, b) => (a._1 + b._1, a._2 + b._2))
      .map { case (id, (d, c)) => AccountTotals(id, d, c) }.toList
  }

  private def v(a: AmountMinor): Long = a.value

  test("trial balance debits equal credits and skips empty accounts") {
    val tb = FinancialReports.trialBalance(dec, usd, chart, totals)
    assertEquals(v(tb.totalDebitMinor), v(tb.totalCreditMinor))
    assertEquals(tb.rows.map(_.code.value: String), tb.rows.map(_.code.value: String).sorted)
    assert(!tb.rows.exists(_.accountId == unused.id))
    val cashRow = tb.rows.find(_.accountId == cash.id).get
    assertEquals(v(cashRow.debitMinor), 50_000L + 20_000L + 12_000L - 600L - 9_000L + 150L - 50L - 1_000L)
    assertEquals(v(cashRow.creditMinor), 0L)
  }

  test("profit and loss computes gross, operating and net income") {
    val pl = FinancialReports.profitAndLoss(jan, dec, usd, chart, totals)
    assertEquals(v(pl.income.totalMinor), 16_000L)
    assertEquals(v(pl.costOfSales.totalMinor), 600L)
    assertEquals(v(pl.grossProfitMinor), 15_400L)
    assertEquals(v(pl.expenses.totalMinor), 9_000L)
    assertEquals(v(pl.operatingIncomeMinor), 6_400L)
    assertEquals(v(pl.otherIncome.totalMinor), 150L)
    assertEquals(v(pl.otherExpenses.totalMinor), 50L)
    assertEquals(v(pl.netIncomeMinor), 6_500L)
  }

  test("balance sheet balances with net income as retained earnings") {
    val bs = FinancialReports.balanceSheet(dec, usd, chart, totals)
    assertEquals(v(bs.retainedEarningsMinor), 6_500L)
    assertEquals(v(bs.fixedAssets.totalMinor), 3_000L)
    assertEquals(v(bs.currentLiabilities.totalMinor), 2_000L)
    assertEquals(v(bs.longTermLiabilities.totalMinor), 20_000L)
    assertEquals(v(bs.totalEquityMinor), 56_500L)
    assertEquals(v(bs.totalAssetsMinor), v(bs.totalLiabilitiesAndEquityMinor))
  }

  test("general ledger carries opening balance and running totals") {
    val entry = JournalEntryId.assume(UUID.randomUUID())
    def row(side: Side, amt: Long, day: Int) =
      LedgerRow(entry, LocalDate.of(2026, 2, day), None, None, cash.id, side, AmountMinor.assume(amt))

    val opening = List(AccountTotals(cash.id, 1_000L, 200L))
    val rows    = List(row(Side.Debit, 500, 3), row(Side.Credit, 300, 9), row(Side.Credit, 1_200, 20))
    val gl      = FinancialReports.generalLedger(LocalDate.of(2026, 2, 1), LocalDate.of(2026, 2, 28), usd, chart, opening, rows)

    assertEquals(gl.accounts.map(_.accountId), List(cash.id))
    val cashGl = gl.accounts.head
    assertEquals(v(cashGl.openingMinor), 800L)
    assertEquals(cashGl.lines.map(l => v(l.balanceMinor)), List(1_300L, 1_000L, -200L))
    assertEquals(v(cashGl.closingMinor), -200L)
  }
}
