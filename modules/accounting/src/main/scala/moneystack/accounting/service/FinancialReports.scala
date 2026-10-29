package moneystack.accounting.service

import moneystack.domain.*
import moneystack.accounting.domain.*

import java.time.LocalDate

/**
 * Financial statements as pure functions over the chart of accounts and
 * per-account totals. All amounts are in the ledger's base currency.
 */
object FinancialReports {

  final case class Line(accountId: AccountId, code: AccountCode, name: Title, amountMinor: AmountMinor)
  final case class Section(lines: List[Line], totalMinor: AmountMinor)

  final case class TrialBalanceRow(
      accountId: AccountId,
      code: AccountCode,
      name: Title,
      accountType: AccountType,
      debitMinor: AmountMinor,
      creditMinor: AmountMinor
  )

  final case class TrialBalance(
      asOf: LocalDate,
      currency: CurrencyCode,
      rows: List[TrialBalanceRow],
      totalDebitMinor: AmountMinor,
      totalCreditMinor: AmountMinor
  )

  final case class ProfitAndLoss(
      from: LocalDate,
      to: LocalDate,
      currency: CurrencyCode,
      income: Section,
      costOfSales: Section,
      grossProfitMinor: AmountMinor,
      expenses: Section,
      operatingIncomeMinor: AmountMinor,
      otherIncome: Section,
      otherExpenses: Section,
      netIncomeMinor: AmountMinor
  )

  final case class BalanceSheet(
      asOf: LocalDate,
      currency: CurrencyCode,
      currentAssets: Section,
      fixedAssets: Section,
      nonCurrentAssets: Section,
      totalAssetsMinor: AmountMinor,
      currentLiabilities: Section,
      longTermLiabilities: Section,
      totalLiabilitiesMinor: AmountMinor,
      equity: Section,
      retainedEarningsMinor: AmountMinor,
      totalEquityMinor: AmountMinor,
      totalLiabilitiesAndEquityMinor: AmountMinor
  )

  final case class GeneralLedgerLine(
      entryId: JournalEntryId,
      date: LocalDate,
      reference: Option[JournalRef],
      memo: Option[Body],
      debitMinor: AmountMinor,
      creditMinor: AmountMinor,
      balanceMinor: AmountMinor
  )

  final case class GeneralLedgerAccount(
      accountId: AccountId,
      code: AccountCode,
      name: Title,
      openingMinor: AmountMinor,
      lines: List[GeneralLedgerLine],
      closingMinor: AmountMinor
  )

  final case class GeneralLedger(from: LocalDate, to: LocalDate, currency: CurrencyCode, accounts: List[GeneralLedgerAccount])

  /** Net balance of every account, placed in its debit or credit column. */
  def trialBalance(asOf: LocalDate, currency: CurrencyCode, accounts: List[Account], totals: List[AccountTotals]): TrialBalance = {
    val byId = totals.map(t => t.accountId -> t).toMap
    val rows = sorted(accounts).flatMap { a =>
      byId.get(a.id).map(t => t.debitMinor - t.creditMinor).filter(_ != 0L).map { net =>
        TrialBalanceRow(a.id, a.code, a.name, a.accountType, minor(net.max(0L)), minor((-net).max(0L)))
      }
    }
    TrialBalance(asOf, currency, rows, minor(rows.map(_.debitMinor.value: Long).sum), minor(rows.map(_.creditMinor.value: Long).sum))
  }

  def profitAndLoss(from: LocalDate, to: LocalDate, currency: CurrencyCode, accounts: List[Account], totals: List[AccountTotals]): ProfitAndLoss = {
    val s           = sectionOf(accounts, totals)
    val income      = s(Set(AccountType.Income))
    val costOfSales = s(Set(AccountType.CostOfSales))
    val expenses    = s(Set(AccountType.Expense))
    val otherIncome = s(Set(AccountType.OtherIncome))
    val otherExp    = s(Set(AccountType.OtherExpense))
    val gross       = income.totalMinor.value - costOfSales.totalMinor.value
    val operating   = gross - expenses.totalMinor.value
    val net         = operating + otherIncome.totalMinor.value - otherExp.totalMinor.value
    ProfitAndLoss(from, to, currency, income, costOfSales, minor(gross), expenses, minor(operating),
                  otherIncome, otherExp, minor(net))
  }

  /**
   * Income and expense accounts are not closed into equity, so cumulative
   * net income through `asOf` appears as retained earnings. Assets always
   * equal liabilities plus equity for a balanced ledger.
   */
  def balanceSheet(asOf: LocalDate, currency: CurrencyCode, accounts: List[Account], totals: List[AccountTotals]): BalanceSheet = {
    val s = sectionOf(accounts, totals)
    import AccountType.*
    val currentAssets       = s(Set(Cash, Bank, AccountsReceivable, Inventory, OtherCurrentAsset))
    val fixedAssets         = s(Set(FixedAsset))
    val nonCurrentAssets    = s(Set(NonCurrentAsset))
    val currentLiabilities  = s(Set(AccountsPayable, CreditCard, TaxPayable, OtherCurrentLiability))
    val longTermLiabilities = s(Set(LongTermLiability))
    val equity              = s(Set(Equity))

    val retained         = netIncome(accounts, totals)
    val totalAssets      = sum(currentAssets, fixedAssets, nonCurrentAssets)
    val totalLiabilities = sum(currentLiabilities, longTermLiabilities)
    val totalEquity      = equity.totalMinor.value + retained

    BalanceSheet(asOf, currency, currentAssets, fixedAssets, nonCurrentAssets, minor(totalAssets),
                 currentLiabilities, longTermLiabilities, minor(totalLiabilities),
                 equity, minor(retained), minor(totalEquity), minor(totalLiabilities + totalEquity))
  }

  /**
   * Per-account activity with a running balance in the account's normal
   * direction. `opening` holds totals up to the day before `from`.
   */
  def generalLedger(
      from: LocalDate,
      to: LocalDate,
      currency: CurrencyCode,
      accounts: List[Account],
      opening: List[AccountTotals],
      rows: List[LedgerRow]
  ): GeneralLedger = {
    val openingById = opening.map(t => t.accountId -> t).toMap
    val rowsById    = rows.groupBy(_.accountId)

    val ledgers = sorted(accounts).flatMap { a =>
      val open  = openingById.get(a.id).fold(0L)(t => Ledger.balance(a.accountType, t.debitMinor, t.creditMinor))
      val lines = rowsById.getOrElse(a.id, Nil)
      if (open == 0L && lines.isEmpty) None
      else {
        val (closing, out) = lines.foldLeft((open, Vector.empty[GeneralLedgerLine])) { case ((bal, acc), r) =>
          val amount       = r.baseAmountMinor.value: Long
          val (dr, cr)     = if (r.side == Side.Debit) (amount, 0L) else (0L, amount)
          val next         = bal + Ledger.balance(a.accountType, dr, cr)
          (next, acc :+ GeneralLedgerLine(r.entryId, r.date, r.reference, r.memo, minor(dr), minor(cr), minor(next)))
        }
        Some(GeneralLedgerAccount(a.id, a.code, a.name, minor(open), out.toList, minor(closing)))
      }
    }
    GeneralLedger(from, to, currency, ledgers)
  }

  /** Income less expenses across all accounts, from `totals`. */
  def netIncome(accounts: List[Account], totals: List[AccountTotals]): Long = {
    val byId = totals.map(t => t.accountId -> t).toMap
    accounts.foldLeft(0L) { (acc, a) =>
      val bal = byId.get(a.id).fold(0L)(t => Ledger.balance(a.accountType, t.debitMinor, t.creditMinor))
      a.accountType.root match {
        case RootType.Income  => acc + bal
        case RootType.Expense => acc - bal
        case _                => acc
      }
    }
  }

  private def sectionOf(accounts: List[Account], totals: List[AccountTotals])(types: Set[AccountType]): Section = {
    val byId  = totals.map(t => t.accountId -> t).toMap
    val lines = sorted(accounts).filter(a => types.contains(a.accountType)).flatMap { a =>
      byId.get(a.id)
        .map(t => Ledger.balance(a.accountType, t.debitMinor, t.creditMinor))
        .filter(_ != 0L)
        .map(bal => Line(a.id, a.code, a.name, minor(bal)))
    }
    Section(lines, minor(lines.map(_.amountMinor.value: Long).sum))
  }

  private def sorted(accounts: List[Account]): List[Account] = accounts.sortBy(_.code.value: String)

  private def sum(sections: Section*): Long = sections.map(_.totalMinor.value: Long).sum

  private def minor(l: Long): AmountMinor = AmountMinor.assume(l)
}
