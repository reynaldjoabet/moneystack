package wealth.accounting.domain

import wealth.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID
import scala.math.BigDecimal.RoundingMode

// ---------- Chart of accounts ----------

enum RootType {
  case Asset, Liability, Equity, Income, Expense
}

enum Side {
  case Debit, Credit

  def opposite: Side = this match {
    case Debit  => Credit
    case Credit => Debit
  }
}
object Side {
  def parse(s: String): Either[String, Side] = s.toLowerCase match {
    case "debit"  => Right(Debit)
    case "credit" => Right(Credit)
    case o        => Left(s"unknown side: $o")
  }
  def render(s: Side): String = s.toString.toLowerCase
}

/** Account types and the statement section each one lands in. */
enum AccountType(val root: RootType, val key: String) {
  case Cash                  extends AccountType(RootType.Asset, "cash")
  case Bank                  extends AccountType(RootType.Asset, "bank")
  case AccountsReceivable    extends AccountType(RootType.Asset, "accounts_receivable")
  case Inventory             extends AccountType(RootType.Asset, "inventory")
  case OtherCurrentAsset     extends AccountType(RootType.Asset, "other_current_asset")
  case FixedAsset            extends AccountType(RootType.Asset, "fixed_asset")
  case NonCurrentAsset       extends AccountType(RootType.Asset, "non_current_asset")
  case AccountsPayable       extends AccountType(RootType.Liability, "accounts_payable")
  case CreditCard            extends AccountType(RootType.Liability, "credit_card")
  case TaxPayable            extends AccountType(RootType.Liability, "tax_payable")
  case OtherCurrentLiability extends AccountType(RootType.Liability, "other_current_liability")
  case LongTermLiability     extends AccountType(RootType.Liability, "long_term_liability")
  case Equity                extends AccountType(RootType.Equity, "equity")
  case Income                extends AccountType(RootType.Income, "income")
  case OtherIncome           extends AccountType(RootType.Income, "other_income")
  case CostOfSales           extends AccountType(RootType.Expense, "cost_of_sales")
  case Expense               extends AccountType(RootType.Expense, "expense")
  case OtherExpense          extends AccountType(RootType.Expense, "other_expense")

  /** The side on which a positive balance accrues. */
  def normal: Side = root match {
    case RootType.Asset | RootType.Expense                     => Side.Debit
    case RootType.Liability | RootType.Equity | RootType.Income => Side.Credit
  }

  /** Accounts that money moves through: bank feeds and settlements use these. */
  def isMoney: Boolean = this == Cash || this == Bank || this == CreditCard
}
object AccountType {
  def parse(s: String): Either[String, AccountType] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown account type: $s")
  def render(t: AccountType): String = t.key
}

final case class Account(
    id: AccountId,
    code: AccountCode,
    name: Title,
    accountType: AccountType,
    currency: CurrencyCode,
    parentId: Option[AccountId],
    active: Boolean,
    createdAt: Instant
)

// ---------- Journal ----------

/** What produced an entry; `(source, sourceId)` is unique across the journal. */
enum JournalSource(val key: String) {
  case Manual              extends JournalSource("manual")
  case Reversal            extends JournalSource("reversal")
  case Payment             extends JournalSource("payment")
  case Invoice             extends JournalSource("invoice")
  case Bill                extends JournalSource("bill")
  case Receipt             extends JournalSource("receipt")
  case BillPayment         extends JournalSource("bill_payment")
  case FxRealized          extends JournalSource("fx_realized")
  case InventoryIssue      extends JournalSource("inventory_issue")
  case InventoryAdjustment extends JournalSource("inventory_adjustment")
  case BankTransaction     extends JournalSource("bank_transaction")
}
object JournalSource {
  def parse(s: String): Either[String, JournalSource] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown journal source: $s")
  def render(s: JournalSource): String = s.key
}

/** A line as submitted, in the entry's currency. `contactId` tags the counterparty. */
final case class JournalLine(
    accountId: AccountId,
    side: Side,
    amountMinor: PriceMinor,
    contactId: Option[ContactId]
)

/** A persisted line: the submitted amount plus its value in the ledger's base currency. */
final case class PostedLine(
    lineNo: Int,
    accountId: AccountId,
    side: Side,
    amountMinor: PriceMinor,
    baseAmountMinor: AmountMinor,
    contactId: Option[ContactId]
)

final case class JournalHeader(
    id: JournalEntryId,
    date: LocalDate,
    reference: Option[JournalRef],
    memo: Option[Body],
    currency: CurrencyCode,
    exchangeRate: ExchangeRate,
    source: JournalSource,
    sourceId: Option[UUID],
    createdBy: Option[UserId],
    createdAt: Instant
)

/** Entries are immutable once posted; corrections go through a reversing entry. */
final case class JournalEntry(header: JournalHeader, lines: List[PostedLine])

final case class FxRate(currency: CurrencyCode, date: LocalDate, rate: ExchangeRate)

/**
 * Closes the books through `lockThrough`. An optional window inside the locked
 * period can be temporarily reopened for adjustments.
 */
final case class LedgerLock(
    lockThrough: LocalDate,
    unlockFrom: Option[LocalDate],
    unlockTo: Option[LocalDate],
    reason: Body,
    updatedBy: UserId,
    updatedAt: Instant
) {
  def locks(date: LocalDate): Boolean = {
    val reopened = (unlockFrom, unlockTo) match {
      case (Some(from), Some(to)) => !date.isBefore(from) && !date.isAfter(to)
      case _                      => false
    }
    !date.isAfter(lockThrough) && !reopened
  }
}

/** Per-account debit/credit sums in base currency. */
final case class AccountTotals(accountId: AccountId, debitMinor: Long, creditMinor: Long)

/** One posted line joined with its entry header, in base currency (general-ledger input). */
final case class LedgerRow(
    entryId: JournalEntryId,
    date: LocalDate,
    reference: Option[JournalRef],
    memo: Option[Body],
    accountId: AccountId,
    side: Side,
    baseAmountMinor: AmountMinor
)

// ---------- Errors ----------

sealed trait LedgerError
object LedgerError {
  // journal
  case object TooFewLines                                                           extends LedgerError
  final case class Unbalanced(debitMinor: Long, creditMinor: Long)                  extends LedgerError
  final case class UnknownAccount(id: AccountId)                                    extends LedgerError
  final case class InactiveAccount(code: AccountCode)                               extends LedgerError
  final case class CurrencyMismatch(code: AccountCode, account: CurrencyCode, entry: CurrencyCode) extends LedgerError
  final case class PeriodLocked(date: LocalDate, lockThrough: LocalDate)            extends LedgerError
  final case class MissingExchangeRate(currency: CurrencyCode, date: LocalDate)     extends LedgerError
  final case class DuplicateAccountCode(code: AccountCode)                          extends LedgerError
  final case class UnknownParent(id: AccountId)                                     extends LedgerError
  final case class ParentTypeMismatch(parent: RootType, child: RootType)            extends LedgerError
  final case class EntryNotFound(id: JournalEntryId)                                extends LedgerError
  final case class AlreadyReversed(id: JournalEntryId, reversal: JournalEntryId)    extends LedgerError
  final case class DuplicatePosting(source: JournalSource, sourceId: UUID)          extends LedgerError
  final case class InvalidPeriod(from: LocalDate, to: LocalDate)                    extends LedgerError
  // catalog
  final case class DuplicateCode(kind: String, code: String)                        extends LedgerError
  final case class UnknownContact(id: ContactId)                                    extends LedgerError
  final case class InactiveContact(id: ContactId)                                   extends LedgerError
  final case class ContactKindMismatch(id: ContactId, expected: String)             extends LedgerError
  final case class UnknownTaxRate(id: TaxRateId)                                    extends LedgerError
  final case class InactiveTaxRate(code: TaxCode)                                   extends LedgerError
  final case class UnknownItem(id: ItemId)                                          extends LedgerError
  final case class InactiveItem(sku: Sku)                                           extends LedgerError
  final case class NotAnInventoryItem(sku: Sku)                                     extends LedgerError
  final case class WrongAccountType(code: AccountCode, expected: String)            extends LedgerError
  // documents & settlements
  case object NoLines                                                               extends LedgerError
  case object ZeroTotal                                                             extends LedgerError
  final case class LineAccountRequired(lineNo: Int)                                 extends LedgerError
  final case class LinePriceRequired(lineNo: Int)                                   extends LedgerError
  final case class DueBeforeIssue(date: LocalDate, dueDate: LocalDate)              extends LedgerError
  final case class InsufficientStock(sku: Sku, onHand: BigDecimal, requested: BigDecimal) extends LedgerError
  final case class StockAlreadyConsumed(sku: Sku, onHand: BigDecimal, received: BigDecimal) extends LedgerError
  final case class DocumentNotFound(id: DocumentId)                                 extends LedgerError
  final case class SettlementNotFound(id: SettlementId)                             extends LedgerError
  final case class AlreadyVoided(on: LocalDate)                                     extends LedgerError
  case object DocumentHasPayments                                                   extends LedgerError
  final case class VoidBeforeIssue(issued: LocalDate, voided: LocalDate)            extends LedgerError
  final case class AllocationMismatch(documentId: DocumentId, reason: String)       extends LedgerError
  final case class OverAllocation(documentId: DocumentId, openMinor: Long, requestedMinor: Long) extends LedgerError
  // banking
  final case class BankTransactionNotFound(id: BankTransactionId)                  extends LedgerError
  final case class InvalidTransactionState(status: String, action: String)          extends LedgerError
  final case class InvalidRule(reason: String)                                      extends LedgerError
}

// ---------- Rules ----------

object Ledger {

  /**
   * An entry needs two or more lines whose debits equal its credits, all on
   * active accounts (reversals may touch since-deactivated ones). A
   * foreign-currency account only takes entries in its own currency;
   * base-currency accounts take any currency at the entry's rate.
   */
  def validate(
      lines: List[JournalLine],
      entryCurrency: CurrencyCode,
      baseCurrency: CurrencyCode,
      accounts: Map[AccountId, Account],
      allowInactive: Boolean = false
  ): Either[LedgerError, Unit] = {
    val debit  = total(lines, Side.Debit)
    val credit = total(lines, Side.Credit)

    def check(l: JournalLine): Option[LedgerError] = accounts.get(l.accountId) match {
      case None                                     => Some(LedgerError.UnknownAccount(l.accountId))
      case Some(a) if !a.active && !allowInactive   => Some(LedgerError.InactiveAccount(a.code))
      case Some(a) if a.currency != baseCurrency && a.currency != entryCurrency =>
        Some(LedgerError.CurrencyMismatch(a.code, a.currency, entryCurrency))
      case Some(_)                                  => None
    }

    if (lines.sizeIs < 2) Left(LedgerError.TooFewLines)
    else if (debit != credit) Left(LedgerError.Unbalanced(debit, credit))
    else lines.iterator.map(check).collectFirst { case Some(err) => err }.toLeft(())
  }

  /** Validates `lines` and values them at the header's rate. */
  def entry(
      header: JournalHeader,
      lines: List[JournalLine],
      baseCurrency: CurrencyCode,
      accounts: Map[AccountId, Account],
      allowInactive: Boolean = false
  ): Either[LedgerError, JournalEntry] =
    validate(lines, header.currency, baseCurrency, accounts, allowInactive)
      .map(_ => JournalEntry(header, toBase(lines, header.exchangeRate)))

  /** `round(minor * rate)`, half-even. */
  def convert(minor: Long, rate: ExchangeRate): Long =
    (BigDecimal(minor) * rate.value).setScale(0, RoundingMode.HALF_EVEN).toLongExact

  /**
   * Converts each line to base currency. Lines are rounded half-even, then
   * the rounding drift on each side is absorbed by that side's largest line,
   * so both sides total `round(entryTotal * rate)` and the entry stays
   * balanced in base currency. Deterministic, so a reversal of the same lines
   * at the same rate cancels exactly.
   */
  def toBase(lines: List[JournalLine], rate: ExchangeRate): List[PostedLine] = {
    val indexed = lines.zipWithIndex

    val base: Map[Int, Long] = Side.values.toList.flatMap { side =>
      val ofSide = indexed.filter(_._1.side == side)
      if (ofSide.isEmpty) Nil
      else {
        val rounded = ofSide.map { case (l, i) => i -> convert(minor(l), rate) }
        val drift   = convert(ofSide.map(p => minor(p._1)).sum, rate) - rounded.map(_._2).sum
        val largest = ofSide.maxBy(p => minor(p._1))._2
        rounded.map { case (i, b) => i -> (if (i == largest) b + drift else b) }
      }
    }.toMap

    indexed.map { case (l, i) =>
      PostedLine(i + 1, l.accountId, l.side, l.amountMinor, AmountMinor.assume(base(i)), l.contactId)
    }
  }

  /** The lines that cancel `entry` when posted at its original rate. */
  def reversalOf(entry: JournalEntry): List[JournalLine] =
    entry.lines.map(l => JournalLine(l.accountId, l.side.opposite, l.amountMinor, l.contactId))

  /** Balance in the account's normal direction (positive = the usual state). */
  def balance(accountType: AccountType, debitMinor: Long, creditMinor: Long): Long =
    accountType.normal match {
      case Side.Debit  => debitMinor - creditMinor
      case Side.Credit => creditMinor - debitMinor
    }

  /** A line for `amount` on `side`, or none when the amount is zero. Negative amounts flip the side. */
  def line(accountId: AccountId, side: Side, amount: Long, contactId: Option[ContactId]): Option[JournalLine] =
    if (amount == 0L) None
    else if (amount > 0L) Some(JournalLine(accountId, side, PriceMinor.assume(amount), contactId))
    else Some(JournalLine(accountId, side.opposite, PriceMinor.assume(-amount), contactId))

  private def total(lines: List[JournalLine], side: Side): Long =
    lines.filter(_.side == side).map(minor).sum

  private def minor(l: JournalLine): Long = l.amountMinor.value
}
