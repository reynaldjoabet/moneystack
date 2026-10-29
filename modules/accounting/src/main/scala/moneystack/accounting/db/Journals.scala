package moneystack.accounting.db

import moneystack.domain.*
import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

import java.time.LocalDate
import java.util.UUID

/**
 * Append-only journal. The schema rejects UPDATE/DELETE on posted rows and
 * checks at commit that every entry balances in base currency.
 */
trait Journals[F[_]] {
  /** Posts atomically, honouring the period lock and `(source, sourceId)` idempotency. */
  def post(e: JournalEntry): F[Journals.Outcome]
  /** As [[post]], inside the caller's transaction. */
  def postIn(s: Session[F], e: JournalEntry): F[Journals.Outcome]
  def find(id: JournalEntryId): F[Option[JournalEntry]]
  def findIn(s: Session[F], id: JournalEntryId): F[Option[JournalEntry]]
  def bySourceIn(s: Session[F], source: JournalSource, sourceId: UUID): F[Option[JournalEntryId]]
  def list(from: LocalDate, to: LocalDate, limit: Int): F[List[JournalEntry]]

  def totalsThrough(to: LocalDate): F[List[AccountTotals]]
  def totalsBetween(from: LocalDate, to: LocalDate): F[List[AccountTotals]]
  def rows(from: LocalDate, to: LocalDate): F[List[LedgerRow]]
  /** Net income-account credits per app user (via their contacts), in base currency. */
  def revenueByUser(from: LocalDate, to: LocalDate): F[List[(UserId, Long)]]

  def lock(): F[Option[LedgerLock]]
  def setLock(l: LedgerLock): F[Unit]
  def clearLock(): F[Unit]
}

object Journals {

  sealed trait Outcome
  object Outcome {
    final case class Posted(entry: JournalEntry)          extends Outcome
    final case class Locked(lock: LedgerLock)             extends Outcome
    final case class Duplicate(existing: JournalEntryId) extends Outcome
  }

  import Codecs.{accountTotals as totalsC, journalEntryId as entryIdC, journalHeader as headerC,
                 journalSource as sourceC, ledgerLock as lockC, ledgerLockRow as lockRowD,
                 ledgerRow as rowC, postedLine as lineC, userId as userIdC}
  import skunk.codec.all.{date, int4, int8, uuid}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Journals[F] =
    new Journals[F] {

      def post(e: JournalEntry): F[Outcome] =
        pool.use(s => s.transaction.use(_ => postIn(s, e)))

      def postIn(s: Session[F], e: JournalEntry): F[Outcome] =
        // FOR SHARE on the lock row serialises postings against lock changes.
        s.prepare(Q.lockForShare).flatMap(_.unique(skunk.Void)).flatMap {
          case Some(l) if l.locks(e.header.date) => Concurrent[F].pure(Outcome.Locked(l))
          case _ =>
            s.prepare(Q.insertHeader).flatMap(_.option(e.header)).flatMap {
              case Some(_) =>
                s.prepare(Q.insertLine)
                  .flatMap(pc => e.lines.traverse_(l => pc.execute((e.header.id, l))))
                  .as(Outcome.Posted(e))
              case None =>
                e.header.sourceId match {
                  case Some(sid) => s.prepare(Q.bySource).flatMap(_.unique((e.header.source, sid))).map(Outcome.Duplicate(_))
                  case None      => Concurrent[F].raiseError(new IllegalStateException(s"journal entry ${e.header.id.value} conflicted without a source id"))
                }
            }
        }

      def find(id: JournalEntryId): F[Option[JournalEntry]] =
        pool.use(findIn(_, id))

      def findIn(s: Session[F], id: JournalEntryId): F[Option[JournalEntry]] =
        s.prepare(Q.headerById).flatMap(_.option(id)).flatMap {
          case None    => Concurrent[F].pure(None)
          case Some(h) => s.prepare(Q.linesFor).flatMap(_.stream(id, 64).compile.toList).map(ls => Some(JournalEntry(h, ls)))
        }

      def bySourceIn(s: Session[F], source: JournalSource, sourceId: UUID): F[Option[JournalEntryId]] =
        s.prepare(Q.bySource).flatMap(_.option((source, sourceId)))

      def list(from: LocalDate, to: LocalDate, limit: Int): F[List[JournalEntry]] =
        pool.use { s =>
          for {
            headers <- s.prepare(Q.headersBetween).flatMap(_.stream((from, to, limit), 64).compile.toList)
            lines   <- s.prepare(Q.linesBetween).flatMap(_.stream((from, to, limit), 256).compile.toList)
            byEntry  = lines.groupMap(_._1)(_._2)
          } yield headers.map(h => JournalEntry(h, byEntry.getOrElse(h.id, Nil)))
        }

      def totalsThrough(to: LocalDate): F[List[AccountTotals]] =
        pool.use(_.prepare(Q.totalsThrough).flatMap(_.stream(to, 256).compile.toList))

      def totalsBetween(from: LocalDate, to: LocalDate): F[List[AccountTotals]] =
        pool.use(_.prepare(Q.totalsBetween).flatMap(_.stream((from, to), 256).compile.toList))

      def rows(from: LocalDate, to: LocalDate): F[List[LedgerRow]] =
        pool.use(_.prepare(Q.rows).flatMap(_.stream((from, to), 512).compile.toList))

      def revenueByUser(from: LocalDate, to: LocalDate): F[List[(UserId, Long)]] =
        pool.use(_.prepare(Q.revenueByUser).flatMap(_.stream((from, to), 256).compile.toList))

      def lock(): F[Option[LedgerLock]] =
        pool.use(_.prepare(Q.lock).flatMap(_.unique(skunk.Void)))

      def setLock(l: LedgerLock): F[Unit] =
        pool.use(_.prepare(Q.setLock).flatMap(_.execute(l))).void

      def clearLock(): F[Unit] =
        pool.use(_.execute(Q.clearLock)).void
    }

  private object Q {

    private val headerCols =
      sql"id, entry_date, reference, memo, currency, exchange_rate, source, source_id, created_by, created_at"

    private val lineCols =
      sql"line_no, account_id, side, amount_minor, base_amount_minor, contact_id"

    val insertHeader: Query[JournalHeader, JournalEntryId] =
      sql"""INSERT INTO journal_entries ($headerCols) VALUES ${headerC.values}
            ON CONFLICT (source, source_id) DO NOTHING
            RETURNING id""".query(entryIdC)

    val insertLine: Command[(JournalEntryId, PostedLine)] =
      sql"""INSERT INTO journal_lines (entry_id, $lineCols) VALUES ($entryIdC, $lineC)""".command

    val bySource: Query[(JournalSource, UUID), JournalEntryId] =
      sql"SELECT id FROM journal_entries WHERE source = $sourceC AND source_id = $uuid".query(entryIdC)

    val headerById: Query[JournalEntryId, JournalHeader] =
      sql"SELECT $headerCols FROM journal_entries WHERE id = $entryIdC".query(headerC)

    val linesFor: Query[JournalEntryId, PostedLine] =
      sql"SELECT $lineCols FROM journal_lines WHERE entry_id = $entryIdC ORDER BY line_no".query(lineC)

    private val latestBetween =
      sql"""SELECT id FROM journal_entries
            WHERE entry_date BETWEEN $date AND $date
            ORDER BY entry_date DESC, created_at DESC LIMIT $int4"""

    val headersBetween: Query[(LocalDate, LocalDate, Int), JournalHeader] =
      sql"""SELECT $headerCols FROM journal_entries WHERE id IN ($latestBetween)
            ORDER BY entry_date DESC, created_at DESC""".query(headerC)

    val linesBetween: Query[(LocalDate, LocalDate, Int), (JournalEntryId, PostedLine)] =
      sql"""SELECT entry_id, $lineCols FROM journal_lines WHERE entry_id IN ($latestBetween)
            ORDER BY entry_id, line_no""".query(entryIdC *: lineC)

    private val sums =
      sql"""SELECT l.account_id,
                   COALESCE(SUM(l.base_amount_minor) FILTER (WHERE l.side = 'debit'), 0)::int8,
                   COALESCE(SUM(l.base_amount_minor) FILTER (WHERE l.side = 'credit'), 0)::int8
            FROM journal_lines l JOIN journal_entries e ON e.id = l.entry_id"""

    val totalsThrough: Query[LocalDate, AccountTotals] =
      sql"$sums WHERE e.entry_date <= $date GROUP BY l.account_id".query(totalsC)

    val totalsBetween: Query[(LocalDate, LocalDate), AccountTotals] =
      sql"$sums WHERE e.entry_date BETWEEN $date AND $date GROUP BY l.account_id".query(totalsC)

    val rows: Query[(LocalDate, LocalDate), LedgerRow] =
      sql"""SELECT e.id, e.entry_date, e.reference, e.memo, l.account_id, l.side, l.base_amount_minor
            FROM journal_lines l JOIN journal_entries e ON e.id = l.entry_id
            WHERE e.entry_date BETWEEN $date AND $date
            ORDER BY e.entry_date, e.created_at, e.id, l.line_no""".query(rowC)

    val revenueByUser: Query[(LocalDate, LocalDate), (UserId, Long)] =
      sql"""SELECT c.user_id,
                   SUM(CASE WHEN l.side = 'credit' THEN l.base_amount_minor ELSE -l.base_amount_minor END)::int8
            FROM journal_lines l
              JOIN journal_entries e ON e.id = l.entry_id
              JOIN ledger_accounts a ON a.id = l.account_id
              JOIN contacts c ON c.id = l.contact_id
            WHERE a.account_type IN ('income', 'other_income')
              AND c.user_id IS NOT NULL
              AND e.entry_date BETWEEN $date AND $date
            GROUP BY c.user_id""".query(userIdC *: int8)

    private val lockCols = sql"lock_through, unlock_from, unlock_to, reason, updated_by, updated_at"

    val lockForShare: Query[skunk.Void, Option[LedgerLock]] =
      sql"SELECT $lockCols FROM ledger_lock WHERE id FOR SHARE".query(lockRowD)

    val lock: Query[skunk.Void, Option[LedgerLock]] =
      sql"SELECT $lockCols FROM ledger_lock WHERE id".query(lockRowD)

    val setLock: Command[LedgerLock] =
      sql"UPDATE ledger_lock SET ($lockCols) = ($lockC) WHERE id".command

    val clearLock: Command[skunk.Void] =
      sql"""UPDATE ledger_lock
            SET lock_through = NULL, unlock_from = NULL, unlock_to = NULL,
                reason = NULL, updated_by = NULL, updated_at = NULL
            WHERE id""".command
  }
}
