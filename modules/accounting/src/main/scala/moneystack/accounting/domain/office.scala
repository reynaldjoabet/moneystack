package moneystack.accounting.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit

// ---------- Drafts, history and recurring documents (after Akaunting's document lifecycle) ----------

/**
 * An invoice or bill still being written. Nothing is posted or numbered
 * until it is issued, which turns it into a [[Document]] and removes the draft.
 * `payload` is the JSON of the document request.
 */
final case class Draft(id: DraftId, kind: DocumentKind, contactId: ContactId, payload: String, createdBy: UserId,
                       createdAt: Instant, updatedAt: Instant)

enum DocumentEventKind(val key: String) {
  case Issued   extends DocumentEventKind("issued")
  case Sent     extends DocumentEventKind("sent")
  case Payment  extends DocumentEventKind("payment")
  case Reminder extends DocumentEventKind("reminder")
  case Voided   extends DocumentEventKind("voided")
}
object DocumentEventKind {
  def parse(s: String): Either[String, DocumentEventKind] =
    values.find(_.key == s.toLowerCase).toRight(s"unknown document event: $s")
  def render(k: DocumentEventKind): String = k.key
}

final case class DocumentEvent(documentId: DocumentId, kind: DocumentEventKind, note: Option[Body], by: Option[UserId], at: Instant)

enum Every(val key: String, val unit: ChronoUnit) {
  case Day   extends Every("daily", ChronoUnit.DAYS)
  case Week  extends Every("weekly", ChronoUnit.WEEKS)
  case Month extends Every("monthly", ChronoUnit.MONTHS)
  case Year  extends Every("yearly", ChronoUnit.YEARS)
}
object Every {
  def parse(s: String): Either[String, Every] = values.find(_.key == s.toLowerCase).toRight(s"unknown frequency: $s")
  def render(e: Every): String               = e.key
}

/**
 * A template issued every `interval` x `every` from `startDate`. Each issue
 * keeps the template's gap between issue and due date. Occurrence n is
 * issued at most once.
 */
final case class Recurring(
    id: RecurringId,
    kind: DocumentKind,
    contactId: ContactId,
    payload: String,
    every: Every,
    interval: Int,
    startDate: LocalDate,
    endDate: Option[LocalDate],
    maxOccurrences: Option[Int],
    nextIndex: Int,
    active: Boolean,
    createdBy: UserId,
    createdAt: Instant
) {
  def due(n: Int): Option[LocalDate] =
    Option.when(n >= 0 && maxOccurrences.forall(n < _))(startDate.plus(n.toLong * interval, every.unit))
      .filter(d => endDate.forall(!d.isAfter(_)))
}

// ---------- Reconciliation ----------

enum ReconciliationStatus(val key: String) {
  case Open   extends ReconciliationStatus("open")
  case Closed extends ReconciliationStatus("closed")
}
object ReconciliationStatus {
  def parse(s: String): Either[String, ReconciliationStatus] = values.find(_.key == s.toLowerCase).toRight(s"unknown status: $s")
  def render(s: ReconciliationStatus): String               = s.key
}

/**
 * Matching a bank statement against the ledger: lines on the account up to
 * `periodEnd` are ticked off until opening balance plus ticked lines equals
 * the statement's closing balance. Amounts are in the account's currency.
 */
final case class Reconciliation(
    id: ReconciliationId,
    accountId: AccountId,
    periodStart: LocalDate,
    periodEnd: LocalDate,
    openingMinor: Long,
    closingMinor: Long,
    status: ReconciliationStatus,
    createdBy: UserId,
    createdAt: Instant,
    closedAt: Option[Instant]
)

/** A ledger line on the reconciled account, signed in its normal direction (debits increase a bank account). */
final case class ReconLine(entryId: JournalEntryId, lineNo: Int, date: LocalDate, reference: Option[JournalRef], memo: Option[Body],
                           amountMinor: Long, cleared: Boolean)

final case class ReconciliationView(reconciliation: Reconciliation, lines: List[ReconLine], clearedMinor: Long, differenceMinor: Long)

object Reconcile {

  /** Statement closing balance less opening balance and ticked lines; zero means the statement agrees. */
  def difference(r: Reconciliation, lines: List[ReconLine]): (Long, Long) = {
    val cleared = lines.filter(_.cleared).map(_.amountMinor).sum
    (cleared, r.closingMinor - r.openingMinor - cleared)
  }
}
