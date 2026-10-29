package moneystack.budget.db

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.platform.{CoreCodecs, FxQuote}

import cats.syntax.all.*
import io.circe.parser.decode
import io.circe.syntax.*

import _root_.skunk.Codec
import _root_.skunk.codec.all.*
import _root_.skunk.data.Arr

import java.util.UUID

object Codecs {

  export CoreCodecs.{body, currency, fine, instant, title, userId}

  private def ids[A](wrap: UUID => A, unwrap: A => UUID): Codec[List[A]] =
    _uuid.imap(_.flattenTo(List).map(wrap))(l => Arr.fromFoldable(l.map(unwrap)))

  val texts: Codec[List[String]] = _text.imap(_.flattenTo(List))(l => Arr.fromFoldable(l))

  val walletId: Codec[WalletId]           = uuid.imap(WalletId.assume)(_.value)
  val categoryId: Codec[CategoryId]       = uuid.imap(CategoryId.assume)(_.value)
  val payeeId: Codec[PayeeId]             = uuid.imap(PayeeId.assume)(_.value)
  val tagId: Codec[TagId]                 = uuid.imap(TagId.assume)(_.value)
  val transactionId: Codec[TransactionId] = uuid.imap(TransactionId.assume)(_.value)
  val budgetId: Codec[BudgetId]           = uuid.imap(BudgetId.assume)(_.value)
  val scheduleId: Codec[ScheduleId]       = uuid.imap(ScheduleId.assume)(_.value)
  val objectiveId: Codec[ObjectiveId]     = uuid.imap(ObjectiveId.assume)(_.value)
  val ruleId: Codec[RuleId]               = uuid.imap(RuleId.assume)(_.value)

  val walletIds: Codec[List[WalletId]]     = ids(WalletId.assume, _.value)
  val categoryIds: Codec[List[CategoryId]] = ids(CategoryId.assume, _.value)
  val tagIds: Codec[List[TagId]]           = ids(TagId.assume, _.value)
  val budgetIds: Codec[List[BudgetId]]     = ids(BudgetId.assume, _.value)

  val walletKind: Codec[WalletKind]         = CoreCodecs.enumOf(16, WalletKind.parse, WalletKind.render)
  val categoryKind: Codec[CategoryKind]     = CoreCodecs.enumOf(8, CategoryKind.parse, CategoryKind.render)
  val txKind: Codec[TxKind]                 = CoreCodecs.enumOf(8, TxKind.parse, TxKind.render)
  val categorySource: Codec[CategorySource] = CoreCodecs.enumOf(8, CategorySource.parse, CategorySource.render)
  val recurrence: Codec[Recurrence]         = CoreCodecs.enumOf(8, Recurrence.parse, Recurrence.render)
  val scheduleKind: Codec[ScheduleKind]     = CoreCodecs.enumOf(16, ScheduleKind.parse, ScheduleKind.render)
  val frequency: Codec[Frequency]           = CoreCodecs.enumOf(8, Frequency.parse, Frequency.render)
  val objectiveKind: Codec[ObjectiveKind]   = CoreCodecs.enumOf(8, ObjectiveKind.parse, ObjectiveKind.render)

  val wallet: Codec[Wallet] =
    (walletId *: userId *: title *: walletKind *: currency *: int8 *: bool *: instant).to[Wallet]

  val share: Codec[WalletShare] = (walletId *: userId *: instant).to[WalletShare]

  val category: Codec[Category] =
    (categoryId *: userId *: title *: categoryKind *: categoryId.opt *: varchar(16).opt *: bool).to[Category]

  val payee: Codec[Payee] = (payeeId *: userId *: title *: texts *: categoryId.opt).to[Payee]

  val tag: Codec[Tag] = (tagId *: userId *: title).to[Tag]

  /** A transaction without its splits, which live in `transaction_splits`. */
  val transaction: Codec[Transaction] =
    (transactionId *: userId *: walletId *: txKind *: int8 *: date *: title *: body.opt *: categoryId.opt *: categorySource *:
      payeeId.opt *: tagIds *: walletId.opt *: int8.opt *: transactionId.opt *: objectiveId.opt *: scheduleId.opt *: date.opt *:
      budgetIds *: varchar(64).opt *: userId *: instant).imap {
      case (id, owner, wallet, kind, amt, d, t, note, cat, src, payee, tags, to, toAmt, refund, obj, sched, due, excl, key, by, at) =>
        Transaction(id, owner, wallet, kind, amt, d, t, note, cat, src, payee, tags, Nil, to, toAmt, refund, obj, sched, due, excl, key, by, at)
    }(t => (t.id, t.ownerId, t.walletId, t.kind, t.amountMinor, t.date, t.title, t.note, t.categoryId, t.categorySource, t.payeeId,
            t.tags, t.toWalletId, t.toAmountMinor, t.refundOf, t.objectiveId, t.scheduleId, t.dueDate, t.excludedBudgets, t.importKey,
            t.createdBy, t.createdAt))

  val split: Codec[Split] = (categoryId *: int8 *: body.opt).to[Split]

  /** A budget without its category limits, which live in `category_limits`. */
  val budget: Codec[Budget] =
    (budgetId *: userId *: title *: int8 *: currency *: recurrence *: int4 *: date *: date.opt *: walletIds *: categoryIds *:
      categoryIds *: bool *: bool *: bool *: bool *: instant).imap {
      case (id, owner, name, amt, ccy, rec, len, start, end, ws, cs, ex, income, roll, pinned, archived, at) =>
        Budget(id, owner, name, amt, ccy, rec, len, start, end, ws, cs, ex, income, roll, Nil, pinned, archived, at)
    }(b => (b.id, b.ownerId, b.name, b.amountMinor, b.currency, b.recurrence, b.periodLength, b.startDate, b.endDate, b.walletIds,
            b.categoryIds, b.excludedCategoryIds, b.income, b.rollover, b.pinned, b.archived, b.createdAt))

  val limit: Codec[CategoryLimit] = (categoryId *: int8).to[CategoryLimit]

  val schedule: Codec[Schedule] =
    (scheduleId *: userId *: scheduleKind *: title *: walletId *: txKind *: int8 *: categoryId.opt *: payeeId.opt *: body.opt *:
      frequency *: int4 *: date *: date.opt *: int4.opt *: bool *: int4 *: int4 *: bool *: instant).to[Schedule]

  val objective: Codec[Objective] =
    (objectiveId *: userId *: objectiveKind *: title *: int8 *: currency *: date.opt *: title.opt *: bool *: instant).to[Objective]

  private def json[A: io.circe.Encoder: io.circe.Decoder]: Codec[A] =
    text.eimap(s => decode[A](s).leftMap(_.getMessage))(_.asJson.noSpaces)

  import RuleJson.given

  val rule: Codec[Rule] =
    (ruleId *: userId *: title *: int4 *: bool *: bool *: json[List[Condition]] *: json[List[Action]] *: int8 *: instant).to[Rule]

  val fxQuote: Codec[FxQuote] = (currency *: currency *: date *: fine).to[FxQuote]
}
