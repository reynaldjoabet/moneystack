package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

import java.time.LocalDate

/** A ledger line on an account, raw, for reconciliation. */
final case class AccountLine(
    entryId: JournalEntryId,
    lineNo: Int,
    date: LocalDate,
    reference: Option[JournalRef],
    memo: Option[moneystack.domain.Body],
    side: Side,
    amountMinor: Long,
    baseAmountMinor: Long,
    cleared: Boolean
)

/** Document drafts, history and recurring templates, and bank reconciliations. */
trait Office[F[_]] {
  def saveDraft(d: Draft): F[Draft]
  def drafts(kind: DocumentKind): F[List[Draft]]
  def findDraft(id: DraftId): F[Option[Draft]]
  def deleteDraftIn(s: Session[F], id: DraftId): F[Boolean]

  def addEvent(e: DocumentEvent): F[Unit]
  def events(document: DocumentId): F[List[DocumentEvent]]

  def createRecurring(r: Recurring): F[Recurring]
  def recurring(): F[List[Recurring]]
  def setRecurringActive(id: RecurringId, active: Boolean): F[Option[Recurring]]

  /** Claims occurrence `n` for `document` and moves the template on, inside the caller's transaction. */
  def claimRunIn(s: Session[F], id: RecurringId, n: Int, document: DocumentId): F[Boolean]

  def createReconciliation(r: Reconciliation): F[Reconciliation]
  def reconciliation(id: ReconciliationId): F[Option[Reconciliation]]
  def reconciliations(account: AccountId): F[List[Reconciliation]]
  def lastClosed(account: AccountId): F[Option[Reconciliation]]
  def accountLines(r: Reconciliation): F[List[AccountLine]]
  def setCleared(id: ReconciliationId, lines: List[(JournalEntryId, Int)], cleared: Boolean): F[Unit]
  def close(id: ReconciliationId, at: java.time.Instant): F[Boolean]
}

object Office {

  import Codecs.{
    accountId as accountIdC, documentEvent as eventC, documentId as documentIdC, documentKind as kindC, draft as draftC,
    draftId as draftIdC, instant, journalEntryId as entryIdC, journalRef, body, recurring as recurringC,
    recurringId as recurringIdC, reconciliation as reconC, reconciliationId as reconIdC, side
  }
  import skunk.codec.all.{bool, date, int4, int8}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Office[F] = new Office[F] {

    def saveDraft(d: Draft): F[Draft] = pool.use(_.prepare(Q.upsertDraft).flatMap(_.unique(d)))

    def drafts(kind: DocumentKind): F[List[Draft]] = pool.use(_.prepare(Q.drafts).flatMap(_.stream(kind, 64).compile.toList))

    def findDraft(id: DraftId): F[Option[Draft]] = pool.use(_.prepare(Q.draft).flatMap(_.option(id)))

    def deleteDraftIn(s: Session[F], id: DraftId): F[Boolean] =
      s.prepare(Q.deleteDraft).flatMap(_.execute(id)).map {
        case skunk.data.Completion.Delete(n) => n > 0
        case _                               => false
      }

    def addEvent(e: DocumentEvent): F[Unit] = pool.use(_.prepare(Q.addEvent).flatMap(_.execute(e))).void

    def events(document: DocumentId): F[List[DocumentEvent]] = pool.use(_.prepare(Q.events).flatMap(_.stream(document, 64).compile.toList))

    def createRecurring(r: Recurring): F[Recurring] = pool.use(_.prepare(Q.insertRecurring).flatMap(_.unique(r)))

    def recurring(): F[List[Recurring]] = pool.use(_.prepare(Q.recurring).flatMap(_.stream(skunk.Void, 64).compile.toList))

    def setRecurringActive(id: RecurringId, active: Boolean): F[Option[Recurring]] =
      pool.use(_.prepare(Q.setRecurringActive).flatMap(_.option((active, id))))

    def claimRunIn(s: Session[F], id: RecurringId, n: Int, document: DocumentId): F[Boolean] =
      s.prepare(Q.claimRun).flatMap(_.option((id, n, document))).flatMap {
        case None    => Concurrent[F].pure(false)
        case Some(_) => s.prepare(Q.advanceRecurring).flatMap(_.execute((n + 1, id, n))).as(true)
      }

    def createReconciliation(r: Reconciliation): F[Reconciliation] = pool.use(_.prepare(Q.insertRecon).flatMap(_.unique(r)))

    def reconciliation(id: ReconciliationId): F[Option[Reconciliation]] = pool.use(_.prepare(Q.recon).flatMap(_.option(id)))

    def reconciliations(account: AccountId): F[List[Reconciliation]] =
      pool.use(_.prepare(Q.recons).flatMap(_.stream(account, 64).compile.toList))

    def lastClosed(account: AccountId): F[Option[Reconciliation]] = pool.use(_.prepare(Q.lastClosed).flatMap(_.option(account)))

    def accountLines(r: Reconciliation): F[List[AccountLine]] =
      pool.use(_.prepare(Q.accountLines).flatMap(_.stream((r.id, r.accountId, r.periodEnd, r.id), 512).compile.toList))

    def setCleared(id: ReconciliationId, lines: List[(JournalEntryId, Int)], cleared: Boolean): F[Unit] =
      pool.use { s =>
        s.transaction.use { _ =>
          if (cleared) s.prepare(Q.clear).flatMap(pc => lines.traverse_ { case (e, n) => pc.execute((id, e, n)) })
          else s.prepare(Q.unclear).flatMap(pc => lines.traverse_ { case (e, n) => pc.execute((id, e, n)) })
        }
      }

    def close(id: ReconciliationId, at: java.time.Instant): F[Boolean] =
      pool.use(_.prepare(Q.close).flatMap(_.execute((at, id)))).map {
        case skunk.data.Completion.Update(n) => n > 0
        case _                               => false
      }
  }

  private object Q {

    private val draftCols = sql"id, kind, contact_id, payload, created_by, created_at, updated_at"

    val upsertDraft: Query[Draft, Draft] =
      sql"""INSERT INTO document_drafts ($draftCols) VALUES ${draftC.values}
            ON CONFLICT (id) DO UPDATE SET contact_id = EXCLUDED.contact_id, payload = EXCLUDED.payload, updated_at = EXCLUDED.updated_at
            RETURNING $draftCols""".query(draftC)

    val drafts: Query[DocumentKind, Draft] =
      sql"SELECT $draftCols FROM document_drafts WHERE kind = $kindC ORDER BY updated_at DESC".query(draftC)

    val draft: Query[DraftId, Draft] = sql"SELECT $draftCols FROM document_drafts WHERE id = $draftIdC".query(draftC)

    val deleteDraft: Command[DraftId] = sql"DELETE FROM document_drafts WHERE id = $draftIdC".command

    val addEvent: Command[DocumentEvent] =
      sql"INSERT INTO document_events (document_id, kind, note, by_user, at) VALUES ${eventC.values}".command

    val events: Query[DocumentId, DocumentEvent] =
      sql"SELECT document_id, kind, note, by_user, at FROM document_events WHERE document_id = $documentIdC ORDER BY at, id".query(eventC)

    private val recurringCols =
      sql"id, kind, contact_id, payload, every, every_n, start_date, end_date, max_occurrences, next_index, active, created_by, created_at"

    val insertRecurring: Query[Recurring, Recurring] =
      sql"INSERT INTO recurring_documents ($recurringCols) VALUES ${recurringC.values} RETURNING $recurringCols".query(recurringC)

    val recurring: Query[skunk.Void, Recurring] =
      sql"SELECT $recurringCols FROM recurring_documents ORDER BY created_at".query(recurringC)

    val setRecurringActive: Query[(Boolean, RecurringId), Recurring] =
      sql"UPDATE recurring_documents SET active = $bool WHERE id = $recurringIdC RETURNING $recurringCols".query(recurringC)

    val claimRun: Query[(RecurringId, Int, DocumentId), RecurringId] =
      sql"""INSERT INTO recurring_runs (recurring_id, occurrence, document_id) VALUES ($recurringIdC, $int4, $documentIdC)
            ON CONFLICT DO NOTHING RETURNING recurring_id""".query(recurringIdC)

    val advanceRecurring: Command[(Int, RecurringId, Int)] =
      sql"UPDATE recurring_documents SET next_index = $int4 WHERE id = $recurringIdC AND next_index = $int4".command

    private val reconCols =
      sql"id, account_id, period_start, period_end, opening_minor, closing_minor, status, created_by, created_at, closed_at"

    val insertRecon: Query[Reconciliation, Reconciliation] =
      sql"INSERT INTO reconciliations ($reconCols) VALUES ${reconC.values} RETURNING $reconCols".query(reconC)

    val recon: Query[ReconciliationId, Reconciliation] = sql"SELECT $reconCols FROM reconciliations WHERE id = $reconIdC".query(reconC)

    val recons: Query[AccountId, Reconciliation] =
      sql"SELECT $reconCols FROM reconciliations WHERE account_id = $accountIdC ORDER BY period_end DESC".query(reconC)

    val lastClosed: Query[AccountId, Reconciliation] =
      sql"""SELECT $reconCols FROM reconciliations WHERE account_id = $accountIdC AND status = 'closed'
            ORDER BY period_end DESC, closed_at DESC LIMIT 1""".query(reconC)

    /** Lines up to the period end not ticked off elsewhere, with whether this reconciliation ticked them. */
    val accountLines: Query[(ReconciliationId, AccountId, LocalDate, ReconciliationId), AccountLine] =
      sql"""SELECT l.entry_id, l.line_no, e.entry_date, e.reference, e.memo, l.side, l.amount_minor, l.base_amount_minor,
                   (r.reconciliation_id = $reconIdC) AS cleared
            FROM journal_lines l
              JOIN journal_entries e ON e.id = l.entry_id
              LEFT JOIN reconciled_lines r ON r.entry_id = l.entry_id AND r.line_no = l.line_no
            WHERE l.account_id = $accountIdC AND e.entry_date <= $date
              AND (r.reconciliation_id IS NULL OR r.reconciliation_id = $reconIdC)
            ORDER BY e.entry_date, e.created_at, l.line_no"""
        .query(entryIdC *: int4 *: date *: journalRef.opt *: body.opt *: side *: int8 *: int8 *: bool.opt)
        .map { case (e, n, d, ref, memo, sd, amt, base, cleared) =>
          AccountLine(e, n, d, ref, memo, sd, amt, base, cleared.getOrElse(false))
        }

    val clear: Command[(ReconciliationId, JournalEntryId, Int)] =
      sql"""INSERT INTO reconciled_lines (reconciliation_id, entry_id, line_no) VALUES ($reconIdC, $entryIdC, $int4)
            ON CONFLICT (entry_id, line_no) DO NOTHING""".command

    val unclear: Command[(ReconciliationId, JournalEntryId, Int)] =
      sql"DELETE FROM reconciled_lines WHERE reconciliation_id = $reconIdC AND entry_id = $entryIdC AND line_no = $int4".command

    val close: Command[(java.time.Instant, ReconciliationId)] =
      sql"UPDATE reconciliations SET status = 'closed', closed_at = $instant WHERE id = $reconIdC AND status = 'open'".command
  }
}
