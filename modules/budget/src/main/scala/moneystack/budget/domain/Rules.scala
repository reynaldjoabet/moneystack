package moneystack.budget.domain

import moneystack.domain.*

import java.time.Instant

// ---------- Automation rules ----------

enum TextField(val key: String) {
  case Title extends TextField("title")
  case Note  extends TextField("note")
}
object TextField extends Keyed[TextField] { def key(a: TextField) = a.key }

enum TextOp(val key: String) {
  case ContainsAny    extends TextOp("contains_any")
  case NotContainsAny extends TextOp("not_contains_any")
  case StartsWithAny  extends TextOp("starts_with_any")
  case EndsWithAny    extends TextOp("ends_with_any")
  case EqualsAny      extends TextOp("equals_any")
  case IsEmpty        extends TextOp("is_empty")
}
object TextOp extends Keyed[TextOp] { def key(a: TextOp) = a.key }

enum AmountOp(val key: String) {
  case Gte     extends AmountOp("gte")
  case Lte     extends AmountOp("lte")
  case Between extends AmountOp("between")
  case Equals  extends AmountOp("equals")
}
object AmountOp extends Keyed[AmountOp] { def key(a: AmountOp) = a.key }

enum NoteMode(val key: String) {
  case Replace extends NoteMode("replace")
  case Append  extends NoteMode("append")
  case Prepend extends NoteMode("prepend")
}
object NoteMode extends Keyed[NoteMode] { def key(a: NoteMode) = a.key }

/** Text tests are case-insensitive; amounts are minor units of the transaction's currency. */
sealed trait Condition
object Condition {
  final case class Text(field: TextField, op: TextOp, values: List[String])       extends Condition
  final case class Payee(in: Boolean, ids: List[PayeeId])                         extends Condition
  final case class Amount(op: AmountOp, min: Option[Long], max: Option[Long])     extends Condition
  final case class Kind(kind: TxKind)                                             extends Condition
  final case class Wallet(in: Boolean, ids: List[WalletId])                       extends Condition
  final case class DayOfMonth(min: Int, max: Int)                                 extends Condition
}

sealed trait Action
object Action {
  final case class SetCategory(categoryId: CategoryId)     extends Action
  final case class AddTags(tagIds: List[TagId])            extends Action
  final case class SetPayee(payeeId: PayeeId)              extends Action
  final case class SetNote(mode: NoteMode, value: String)  extends Action
}

/** Rules run in `position` order; with `matchAll` every condition must hold, otherwise any. */
final case class Rule(
    id: RuleId,
    ownerId: UserId,
    name: Title,
    position: Int,
    enabled: Boolean,
    matchAll: Boolean,
    conditions: List[Condition],
    actions: List[Action],
    matchCount: Long,
    createdAt: Instant
)

object Rules {

  val MaxConditions = 10
  val MaxActions    = 4
  val MaxKeywords   = 20

  def validate(conditions: List[Condition], actions: List[Action]): Either[BudgetError, Unit] = {
    def bad(msg: String) = Left(BudgetError.Invalid(msg))
    val conditionErrors = conditions.collectFirst {
      case Condition.Text(_, op, vs) if op != TextOp.IsEmpty && (vs.isEmpty || vs.exists(_.trim.isEmpty)) => "text conditions need non-blank keywords"
      case Condition.Text(_, _, vs) if vs.size > MaxKeywords || vs.exists(_.length > 64)                 => s"at most $MaxKeywords keywords of 64 characters"
      case Condition.Amount(AmountOp.Between, min, max) if min.isEmpty || max.isEmpty || min.exists(m => max.exists(m > _)) => "between needs min <= max"
      case Condition.Amount(AmountOp.Gte | AmountOp.Equals, None, _)                                    => "gte and equals need min"
      case Condition.Amount(AmountOp.Lte, _, None)                                                      => "lte needs max"
      case Condition.DayOfMonth(min, max) if min < 1 || max > 31 || min > max                           => "day of month must be within 1-31, min <= max"
      case Condition.Payee(_, ids) if ids.isEmpty                                                       => "payee conditions need ids"
      case Condition.Wallet(_, ids) if ids.isEmpty                                                      => "wallet conditions need ids"
    }
    if (conditions.isEmpty) bad("a rule needs at least one condition")
    else if (conditions.size > MaxConditions) bad(s"at most $MaxConditions conditions")
    else if (actions.isEmpty || actions.size > MaxActions) bad(s"a rule needs 1 to $MaxActions actions")
    else conditionErrors.fold(Right(()))(bad)
  }

  def matches(rule: Rule, tx: Transaction): Boolean = {
    val results = rule.conditions.map(holds(_, tx))
    rule.enabled && (if (rule.matchAll) results.forall(identity) else results.exists(identity))
  }

  private def holds(c: Condition, tx: Transaction): Boolean = c match {
    case Condition.Text(field, op, values) =>
      val text = (field match {
        case TextField.Title => tx.title.value: String
        case TextField.Note  => tx.note.fold("")(n => n.value: String)
      }).toLowerCase
      val vs = values.map(_.toLowerCase)
      op match {
        case TextOp.ContainsAny    => vs.exists(text.contains)
        case TextOp.NotContainsAny => !vs.exists(text.contains)
        case TextOp.StartsWithAny  => vs.exists(text.startsWith)
        case TextOp.EndsWithAny    => vs.exists(text.endsWith)
        case TextOp.EqualsAny      => vs.contains(text)
        case TextOp.IsEmpty        => text.trim.isEmpty
      }
    case Condition.Payee(in, ids)  => tx.payeeId.exists(ids.contains) == in
    case Condition.Amount(op, min, max) =>
      val a = tx.amountMinor
      op match {
        case AmountOp.Gte     => min.exists(a >= _)
        case AmountOp.Lte     => max.exists(a <= _)
        case AmountOp.Between => min.exists(a >= _) && max.exists(a <= _)
        case AmountOp.Equals  => min.contains(a)
      }
    case Condition.Kind(k)          => tx.kind == k
    case Condition.Wallet(in, ids)  => ids.contains(tx.walletId) == in
    case Condition.DayOfMonth(lo, hi) => tx.date.getDayOfMonth >= lo && tx.date.getDayOfMonth <= hi
  }

  /**
   * Applies `rule`'s actions. A category chosen by hand (or by its schedule)
   * is never overwritten; tags are added, not replaced.
   */
  def applyTo(rule: Rule, tx: Transaction): Transaction =
    rule.actions.foldLeft(tx) { (t, action) =>
      action match {
        case Action.SetCategory(id) =>
          if (t.categorySource == CategorySource.Manual || t.categorySource == CategorySource.Schedule || t.kind == TxKind.Transfer || t.splits.nonEmpty) t
          else t.copy(categoryId = Some(id), categorySource = CategorySource.Rule)
        case Action.AddTags(ids)    => t.copy(tags = (t.tags ++ ids).distinct)
        case Action.SetPayee(id)    => t.copy(payeeId = Some(id))
        case Action.SetNote(mode, v) =>
          val current = t.note.fold("")(n => n.value: String)
          val next = mode match {
            case NoteMode.Replace => v
            case NoteMode.Append  => if (current.isEmpty) v else s"$current $v"
            case NoteMode.Prepend => if (current.isEmpty) v else s"$v $current"
          }
          t.copy(note = Body.option(next.take(8000)))
      }
    }

  /** Runs every matching rule in order; returns the result and the rules that matched. */
  def run(rules: List[Rule], tx: Transaction): (Transaction, List[RuleId]) =
    rules.sortBy(_.position).foldLeft((tx, List.empty[RuleId])) { case ((t, hit), r) =>
      if (matches(r, t)) (applyTo(r, t), hit :+ r.id) else (t, hit)
    }
}

// ---------- Payees ----------

object Payees {

  /** Lower-case letters only, single-spaced: "AMZN Mktp US*2K4" and "amzn mktp us" compare equal. */
  def normalize(s: String): String =
    s.toLowerCase.map(c => if (c.isLetter) c else ' ').split("\\s+").filter(_.nonEmpty).mkString(" ")

  /**
   * The payee for a transaction title: an exact match on name or alias wins,
   * then the longest name or alias contained in the title.
   */
  def find(payees: List[Payee], title: String): Option[Payee] = {
    val t = normalize(title)
    payees.find(p => ((p.name.value: String) :: p.aliases).map(normalize).contains(t))
      .orElse {
        payees.flatMap(p => ((p.name.value: String) :: p.aliases).map(normalize).filter(n => n.nonEmpty && containsWord(t, n)).map(n => n.length -> p))
          .maxByOption(_._1).map(_._2)
      }
  }

  private def containsWord(text: String, phrase: String): Boolean =
    s" $text ".contains(s" $phrase ")
}
