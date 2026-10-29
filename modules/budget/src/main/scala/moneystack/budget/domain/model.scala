package moneystack.budget.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}

private[domain] trait Keyed[A] {
  def values: Array[A]
  def key(a: A): String
  def parse(s: String): Either[String, A] = values.find(key(_) == s.toLowerCase).toRight(s"unknown value: $s")
  def render(a: A): String               = key(a)
}

// ---------- Wallets ----------

enum WalletKind(val key: String) {
  case Cash       extends WalletKind("cash")
  case Checking   extends WalletKind("checking")
  case Savings    extends WalletKind("savings")
  case CreditCard extends WalletKind("credit_card")
  case Loan       extends WalletKind("loan")
  case Investment extends WalletKind("investment")
  case Other      extends WalletKind("other")
}
object WalletKind extends Keyed[WalletKind] { def key(a: WalletKind) = a.key }

/** A place money sits. Amounts are minor units of `currency`; credit cards and loans run negative. */
final case class Wallet(
    id: WalletId,
    ownerId: UserId,
    name: Title,
    kind: WalletKind,
    currency: CurrencyCode,
    openingBalanceMinor: Long,
    archived: Boolean,
    createdAt: Instant
)

/** Read-only access to a wallet for another user (household sharing). */
final case class WalletShare(walletId: WalletId, userId: UserId, createdAt: Instant)

// ---------- Categories, payees, tags ----------

enum CategoryKind(val key: String) {
  case Income  extends CategoryKind("income")
  case Expense extends CategoryKind("expense")
}
object CategoryKind extends Keyed[CategoryKind] { def key(a: CategoryKind) = a.key }

/** One level of nesting: a subcategory's totals roll up into its parent. */
final case class Category(
    id: CategoryId,
    ownerId: UserId,
    name: Title,
    kind: CategoryKind,
    parentId: Option[CategoryId],
    colour: Option[String],
    archived: Boolean
)

/** A merchant or counterparty. Titles matching its name or an alias pick it up, and its default category with it. */
final case class Payee(id: PayeeId, ownerId: UserId, name: Title, aliases: List[String], defaultCategoryId: Option[CategoryId])

final case class Tag(id: TagId, ownerId: UserId, name: Title)

// ---------- Transactions ----------

enum TxKind(val key: String) {
  case Income   extends TxKind("income")
  case Expense  extends TxKind("expense")
  case Transfer extends TxKind("transfer")
}
object TxKind extends Keyed[TxKind] { def key(a: TxKind) = a.key }

/** Who set a transaction's category, so rules never overwrite a person's choice. */
enum CategorySource(val key: String) {
  case Manual   extends CategorySource("manual")
  case Payee    extends CategorySource("payee")
  case Rule     extends CategorySource("rule")
  case Schedule extends CategorySource("schedule")
  case Unset    extends CategorySource("unset")
}
object CategorySource extends Keyed[CategorySource] { def key(a: CategorySource) = a.key }

final case class Split(categoryId: CategoryId, amountMinor: Long, note: Option[Body])

/**
 * Income or expense on a wallet, or a transfer from `walletId` to
 * `toWalletId` (`toAmountMinor` differs across currencies). `refundOf`
 * marks income that gives back an earlier expense; `objectiveId` counts the
 * money toward a goal or loan; `scheduleId`/`dueDate` tie it to a recurring
 * payment.
 */
final case class Transaction(
    id: TransactionId,
    ownerId: UserId,
    walletId: WalletId,
    kind: TxKind,
    amountMinor: Long,
    date: LocalDate,
    title: Title,
    note: Option[Body],
    categoryId: Option[CategoryId],
    categorySource: CategorySource,
    payeeId: Option[PayeeId],
    tags: List[TagId],
    splits: List[Split],
    toWalletId: Option[WalletId],
    toAmountMinor: Option[Long],
    refundOf: Option[TransactionId],
    objectiveId: Option[ObjectiveId],
    scheduleId: Option[ScheduleId],
    dueDate: Option[LocalDate],
    excludedBudgets: List[BudgetId],
    importKey: Option[String],
    createdBy: UserId,
    createdAt: Instant
)

// ---------- Budgets ----------

enum Recurrence(val key: String) {
  case Daily   extends Recurrence("daily")
  case Weekly  extends Recurrence("weekly")
  case Monthly extends Recurrence("monthly")
  case Yearly  extends Recurrence("yearly")
  /** One fixed window from `startDate` to `endDate`. */
  case Custom  extends Recurrence("custom")
}
object Recurrence extends Keyed[Recurrence] { def key(a: Recurrence) = a.key }

/**
 * A spending (or, with `income`, earning) target per period. Periods are
 * `periodLength` units of `recurrence` from `startDate`. Empty filters mean
 * all wallets / all categories of the right kind; a category filter takes
 * its subcategories with it. With `rollover`, the previous period's unspent
 * amount is added.
 */
final case class Budget(
    id: BudgetId,
    ownerId: UserId,
    name: Title,
    amountMinor: Long,
    currency: CurrencyCode,
    recurrence: Recurrence,
    periodLength: Int,
    startDate: LocalDate,
    endDate: Option[LocalDate],
    walletIds: List[WalletId],
    categoryIds: List[CategoryId],
    excludedCategoryIds: List[CategoryId],
    income: Boolean,
    rollover: Boolean,
    categoryLimits: List[CategoryLimit],
    pinned: Boolean,
    archived: Boolean,
    createdAt: Instant
)

final case class CategoryLimit(categoryId: CategoryId, amountMinor: Long)

// ---------- Schedules (upcoming, subscriptions, bills, instalments) ----------

enum ScheduleKind(val key: String) {
  case Subscription extends ScheduleKind("subscription")
  case Bill         extends ScheduleKind("bill")
  case Installment  extends ScheduleKind("installment")
  case Repetitive   extends ScheduleKind("repetitive")
}
object ScheduleKind extends Keyed[ScheduleKind] { def key(a: ScheduleKind) = a.key }

enum Frequency(val key: String) {
  case Daily   extends Frequency("daily")
  case Weekly  extends Frequency("weekly")
  case Monthly extends Frequency("monthly")
  case Yearly  extends Frequency("yearly")
}
object Frequency extends Keyed[Frequency] { def key(a: Frequency) = a.key }

/**
 * A recurring transaction. Occurrence n is due `n x interval` units after
 * `startDate` (monthly dates keep the start's day, clamped to short months).
 * It ends after `maxOccurrences` or `endDate`. `nextIndex` is the next unpaid
 * occurrence; `autoPay` records it automatically once due.
 */
final case class Schedule(
    id: ScheduleId,
    ownerId: UserId,
    kind: ScheduleKind,
    title: Title,
    walletId: WalletId,
    txKind: TxKind,
    amountMinor: Long,
    categoryId: Option[CategoryId],
    payeeId: Option[PayeeId],
    note: Option[Body],
    frequency: Frequency,
    interval: Int,
    startDate: LocalDate,
    endDate: Option[LocalDate],
    maxOccurrences: Option[Int],
    autoPay: Boolean,
    remindDaysBefore: Int,
    nextIndex: Int,
    active: Boolean,
    createdAt: Instant
)

// ---------- Objectives ----------

enum ObjectiveKind(val key: String) {
  /** Saving toward a target: linked expenses/transfers add, linked income withdraws. */
  case Goal     extends ObjectiveKind("goal")
  /** Money lent: linked expenses are lent out, linked income is repaid. */
  case Lent     extends ObjectiveKind("lent")
  /** Money borrowed: linked income is received, linked expenses repay it. */
  case Borrowed extends ObjectiveKind("borrowed")
}
object ObjectiveKind extends Keyed[ObjectiveKind] { def key(a: ObjectiveKind) = a.key }

final case class Objective(
    id: ObjectiveId,
    ownerId: UserId,
    kind: ObjectiveKind,
    name: Title,
    targetMinor: Long,
    currency: CurrencyCode,
    dueDate: Option[LocalDate],
    counterparty: Option[Title],
    archived: Boolean,
    createdAt: Instant
)

// ---------- Errors ----------

sealed trait BudgetError
object BudgetError {
  final case class WalletNotFound(id: WalletId)                  extends BudgetError
  final case class CategoryNotFound(id: CategoryId)              extends BudgetError
  final case class PayeeNotFound(id: PayeeId)                    extends BudgetError
  final case class TagNotFound(id: TagId)                        extends BudgetError
  final case class TransactionNotFound(id: TransactionId)        extends BudgetError
  final case class BudgetNotFound(id: BudgetId)                  extends BudgetError
  final case class ScheduleNotFound(id: ScheduleId)              extends BudgetError
  final case class ObjectiveNotFound(id: ObjectiveId)            extends BudgetError
  final case class RuleNotFound(id: RuleId)                      extends BudgetError
  final case class ReadOnlyWallet(id: WalletId)                  extends BudgetError
  final case class Invalid(reason: String)                       extends BudgetError
  final case class MissingFxRate(from: CurrencyCode, to: CurrencyCode) extends BudgetError
  final case class ScheduleFinished(id: ScheduleId)              extends BudgetError
  final case class AlreadyRecorded(id: ScheduleId, due: LocalDate) extends BudgetError
  final case class ImportFailed(errors: List[String])            extends BudgetError
}
