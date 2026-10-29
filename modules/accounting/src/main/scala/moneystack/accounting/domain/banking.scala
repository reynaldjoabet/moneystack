package moneystack.accounting.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

// ---------- Bank feed ----------

enum BankTxStatus(val key: String) {
  case Uncategorized extends BankTxStatus("uncategorized")
  case Categorized   extends BankTxStatus("categorized")
  case Excluded      extends BankTxStatus("excluded")
}
object BankTxStatus {
  def parse(s: String): Either[String, BankTxStatus] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown bank transaction status: $s")
  def render(s: BankTxStatus): String = s.key
}

/**
 * A line from a bank feed, in the bank account's currency. Positive amounts
 * are deposits, negative withdrawals. Categorising posts a journal entry
 * against `categoryAccountId`; `postingId` is that posting's source id.
 */
final case class BankTransaction(
    id: BankTransactionId,
    accountId: AccountId,
    date: LocalDate,
    description: Title,
    amountMinor: Long,
    externalId: Option[String],
    status: BankTxStatus,
    ruleId: Option[BankRuleId],
    categoryAccountId: Option[AccountId],
    contactId: Option[ContactId],
    entryId: Option[JournalEntryId],
    postingId: Option[UUID],
    createdAt: Instant
) {
  def isDeposit: Boolean = amountMinor > 0L
}

// ---------- Rules ----------

enum RuleApplyTo(val key: String) {
  case Deposits    extends RuleApplyTo("deposits")
  case Withdrawals extends RuleApplyTo("withdrawals")
  case Both        extends RuleApplyTo("both")
}
object RuleApplyTo {
  def parse(s: String): Either[String, RuleApplyTo] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown rule target: $s")
  def render(a: RuleApplyTo): String = a.key
}

enum Conjunction(val key: String) {
  case All extends Conjunction("all")
  case Any extends Conjunction("any")
}
object Conjunction {
  def parse(s: String): Either[String, Conjunction] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown conjunction: $s")
  def render(c: Conjunction): String = c.key
}

enum RuleField(val key: String) {
  case Description extends RuleField("description")
  case Amount      extends RuleField("amount")
}
object RuleField {
  def parse(s: String): Either[String, RuleField] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown rule field: $s")
  def render(f: RuleField): String = f.key
}

enum RuleOperator(val key: String) {
  case Contains    extends RuleOperator("contains")
  case Equals      extends RuleOperator("equals")
  case StartsWith  extends RuleOperator("starts_with")
  case EndsWith    extends RuleOperator("ends_with")
  case GreaterThan extends RuleOperator("greater_than")
  case LessThan    extends RuleOperator("less_than")
}
object RuleOperator {
  def parse(s: String): Either[String, RuleOperator] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown rule operator: $s")
  def render(o: RuleOperator): String = o.key
}

/** Description tests are case-insensitive; amount tests compare the absolute amount in minor units. */
final case class RuleCondition(field: RuleField, operator: RuleOperator, value: String)

/**
 * Lower `priority` runs first. A matching rule tags the transaction; with
 * `autoCategorize` it also posts it to `accountId`.
 */
final case class BankRule(
    id: BankRuleId,
    name: Title,
    priority: Int,
    applyTo: RuleApplyTo,
    conjunction: Conjunction,
    conditions: List[RuleCondition],
    accountId: AccountId,
    contactId: Option[ContactId],
    autoCategorize: Boolean,
    active: Boolean,
    createdAt: Instant
)

object BankRules {

  def validate(conditions: List[RuleCondition]): Either[LedgerError, Unit] = {
    def check(c: RuleCondition): Option[String] = (c.field, c.operator) match {
      case (_, _) if c.value.trim.isEmpty => Some("condition value must not be blank")
      case (RuleField.Amount, RuleOperator.Equals | RuleOperator.GreaterThan | RuleOperator.LessThan) =>
        c.value.toLongOption.filter(_ >= 0L).fold(Some(s"amount must be a non-negative integer of minor units: ${c.value}"))(_ => None)
      case (RuleField.Amount, op) => Some(s"${op.key} does not apply to amounts")
      case (RuleField.Description, RuleOperator.GreaterThan | RuleOperator.LessThan) =>
        Some(s"${c.operator.key} does not apply to descriptions")
      case (RuleField.Description, _) => None
    }
    if (conditions.isEmpty) Left(LedgerError.InvalidRule("a rule needs at least one condition"))
    else conditions.iterator.map(check).collectFirst { case Some(msg) => LedgerError.InvalidRule(msg) }.toLeft(())
  }

  def matches(rule: BankRule, tx: BankTransaction): Boolean = {
    val direction = rule.applyTo match {
      case RuleApplyTo.Deposits    => tx.isDeposit
      case RuleApplyTo.Withdrawals => !tx.isDeposit
      case RuleApplyTo.Both        => true
    }
    val results = rule.conditions.map(holds(_, tx))
    direction && (rule.conjunction match {
      case Conjunction.All => results.forall(identity)
      case Conjunction.Any => results.exists(identity)
    })
  }

  /** The first active rule, by priority then age, that matches `tx`. */
  def firstMatch(rules: List[BankRule], tx: BankTransaction): Option[BankRule] =
    rules.filter(_.active).sortBy(r => (r.priority, r.createdAt)).find(matches(_, tx))

  private def holds(c: RuleCondition, tx: BankTransaction): Boolean = c.field match {
    case RuleField.Description =>
      val text  = tx.description.value.toLowerCase
      val value = c.value.toLowerCase
      c.operator match {
        case RuleOperator.Contains   => text.contains(value)
        case RuleOperator.Equals     => text == value
        case RuleOperator.StartsWith => text.startsWith(value)
        case RuleOperator.EndsWith   => text.endsWith(value)
        case _                       => false
      }
    case RuleField.Amount =>
      val amount = math.abs(tx.amountMinor)
      c.value.toLongOption.exists { v =>
        c.operator match {
          case RuleOperator.Equals      => amount == v
          case RuleOperator.GreaterThan => amount > v
          case RuleOperator.LessThan    => amount < v
          case _                        => false
        }
      }
  }
}
