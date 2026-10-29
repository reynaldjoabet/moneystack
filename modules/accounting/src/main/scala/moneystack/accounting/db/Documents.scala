package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

import java.time.LocalDate

/** Invoices and bills. Headers gain only `voided_on` after issue; lines never change. */
trait Documents[F[_]] {
  /** Allocates the next number in `series` inside the caller's transaction (gapless). */
  def nextNumber(s: Session[F], series: String): F[Long]
  def insert(s: Session[F], d: Document, lines: List[DocumentLine]): F[Unit]
  def find(id: DocumentId): F[Option[Document]]
  def findForUpdate(s: Session[F], id: DocumentId): F[Option[Document]]
  def lines(id: DocumentId): F[List[DocumentLine]]
  def linesIn(s: Session[F], id: DocumentId): F[List[DocumentLine]]
  /** Amount and document-rate base already settled by non-voided settlements. */
  def paidIn(s: Session[F], id: DocumentId): F[(Long, Long)]
  def paid(id: DocumentId): F[(Long, Long)]
  def list(kind: DocumentKind, from: LocalDate, to: LocalDate, contact: Option[ContactId], limit: Int): F[List[(Document, Long)]]
  def markVoided(s: Session[F], id: DocumentId, on: LocalDate): F[Unit]

  def openItems(kind: DocumentKind, asOf: LocalDate): F[List[OpenItem]]
  def taxActivity(from: LocalDate, to: LocalDate): F[List[TaxActivity]]
}

object Documents {

  import Codecs.{contactId as contactIdC, document as documentC, documentId as documentIdC,
                 documentKind as kindC, documentLine as lineC, openItem as openItemC, taxRateId as taxRateIdC}
  import skunk.codec.all.{date, int4, int8, varchar}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Documents[F] =
    new Documents[F] {

      def nextNumber(s: Session[F], series: String): F[Long] =
        s.prepare(Q.nextNumber).flatMap(_.unique(series))

      def insert(s: Session[F], d: Document, lines: List[DocumentLine]): F[Unit] =
        for {
          _ <- s.prepare(Q.insert).flatMap(_.execute(d))
          _ <- s.prepare(Q.insertLine).flatMap(pc => lines.traverse_(l => pc.execute((d.id, l))))
        } yield ()

      def find(id: DocumentId): F[Option[Document]] =
        pool.use(_.prepare(Q.byId).flatMap(_.option(id)))

      def findForUpdate(s: Session[F], id: DocumentId): F[Option[Document]] =
        s.prepare(Q.byIdForUpdate).flatMap(_.option(id))

      def lines(id: DocumentId): F[List[DocumentLine]] =
        pool.use(linesIn(_, id))

      def linesIn(s: Session[F], id: DocumentId): F[List[DocumentLine]] =
        s.prepare(Q.lines).flatMap(_.stream(id, 64).compile.toList)

      def paidIn(s: Session[F], id: DocumentId): F[(Long, Long)] =
        s.prepare(Q.paid).flatMap(_.unique(id))

      def paid(id: DocumentId): F[(Long, Long)] =
        pool.use(paidIn(_, id))

      def list(kind: DocumentKind, from: LocalDate, to: LocalDate, contact: Option[ContactId], limit: Int): F[List[(Document, Long)]] =
        pool.use(_.prepare(Q.list).flatMap(_.stream((kind, from, to, contact, contact, limit), 64).compile.toList))

      def markVoided(s: Session[F], id: DocumentId, on: LocalDate): F[Unit] =
        s.prepare(Q.markVoided).flatMap(_.execute((on, id))).void

      def openItems(kind: DocumentKind, asOf: LocalDate): F[List[OpenItem]] =
        pool.use(_.prepare(Q.openItems).flatMap(_.stream((kind, asOf), 256).compile.toList))

      def taxActivity(from: LocalDate, to: LocalDate): F[List[TaxActivity]] =
        pool.use(_.prepare(Q.taxActivity).flatMap(_.stream((from, to, to), 64).compile.toList))
    }

  private object Q {

    private val cols =
      sql"""id, kind, number, contact_id, issue_date, due_date, currency, exchange_rate, reference, memo,
            subtotal_minor, tax_minor, total_minor, control_base_minor, entry_id, voided_on, created_by, created_at"""

    private val lineCols =
      sql"""line_no, item_id, description, quantity, unit_price_minor, account_id, tax_rate_id, tax_bps,
            amount_minor, tax_minor"""

    val nextNumber: Query[String, Long] =
      sql"""UPDATE document_sequences SET next_value = next_value + 1
            WHERE series = ${varchar(16)} RETURNING next_value - 1""".query(int8)

    val insert: Command[Document] =
      sql"INSERT INTO documents ($cols) VALUES ${documentC.values}".command

    val insertLine: Command[(DocumentId, DocumentLine)] =
      sql"INSERT INTO document_lines (document_id, $lineCols) VALUES ($documentIdC, $lineC)".command

    val byId: Query[DocumentId, Document] =
      sql"SELECT $cols FROM documents WHERE id = $documentIdC".query(documentC)

    val byIdForUpdate: Query[DocumentId, Document] =
      sql"SELECT $cols FROM documents WHERE id = $documentIdC FOR UPDATE".query(documentC)

    val lines: Query[DocumentId, DocumentLine] =
      sql"SELECT $lineCols FROM document_lines WHERE document_id = $documentIdC ORDER BY line_no".query(lineC)

    private val activeAllocations =
      sql"""settlement_allocations a JOIN settlements s ON s.id = a.settlement_id AND s.voided_on IS NULL"""

    val paid: Query[DocumentId, (Long, Long)] =
      sql"""SELECT COALESCE(SUM(a.amount_minor), 0)::int8, COALESCE(SUM(a.document_base_minor), 0)::int8
            FROM $activeAllocations WHERE a.document_id = $documentIdC""".query(int8 *: int8)

    val list: Query[(DocumentKind, LocalDate, LocalDate, Option[ContactId], Option[ContactId], Int), (Document, Long)] =
      sql"""SELECT $cols,
                   (SELECT COALESCE(SUM(a.amount_minor), 0)::int8 FROM $activeAllocations WHERE a.document_id = documents.id)
            FROM documents
            WHERE kind = $kindC AND issue_date BETWEEN $date AND $date
              AND (${contactIdC.opt}::uuid IS NULL OR contact_id = ${contactIdC.opt})
            ORDER BY issue_date DESC, number DESC LIMIT $int4""".query(documentC *: int8)

    val markVoided: Command[(LocalDate, DocumentId)] =
      sql"UPDATE documents SET voided_on = $date WHERE id = $documentIdC".command

    /** Settlements count once dated on/before as-of and not voided by then; same for documents. */
    private val settledAsOf =
      sql"""FROM settlement_allocations a JOIN settlements s ON s.id = a.settlement_id
            WHERE a.document_id = d.id AND s.settle_date <= $date AND (s.voided_on IS NULL OR s.voided_on > $date)"""

    val openItems: Query[(DocumentKind, LocalDate), OpenItem] =
      sql"""SELECT d.id, d.number, d.contact_id, d.issue_date, d.due_date, d.currency, d.total_minor,
                   COALESCE((SELECT SUM(a.amount_minor) $settledAsOf), 0)::int8,
                   d.control_base_minor,
                   COALESCE((SELECT SUM(a.document_base_minor) $settledAsOf), 0)::int8
            FROM documents d
            WHERE d.kind = $kindC AND d.issue_date <= $date AND (d.voided_on IS NULL OR d.voided_on > $date)
            ORDER BY d.due_date, d.number"""
        .query(openItemC)
        .contramap[(DocumentKind, LocalDate)] { case (k, d) => ((d, d), (d, d), k, d, d) }

    val taxActivity: Query[(LocalDate, LocalDate, LocalDate), TaxActivity] =
      sql"""SELECT d.kind, l.tax_rate_id,
                   SUM(ROUND(l.amount_minor * d.exchange_rate))::int8,
                   SUM(ROUND(l.tax_minor * d.exchange_rate))::int8
            FROM document_lines l JOIN documents d ON d.id = l.document_id
            WHERE l.tax_rate_id IS NOT NULL
              AND d.issue_date BETWEEN $date AND $date
              AND (d.voided_on IS NULL OR d.voided_on > $date)
            GROUP BY d.kind, l.tax_rate_id""".query((kindC *: taxRateIdC *: int8 *: int8).to[TaxActivity])
  }
}
