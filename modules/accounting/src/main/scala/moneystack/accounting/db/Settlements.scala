package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

import java.time.LocalDate

/** Customer receipts and vendor payments with their allocations. */
trait Settlements[F[_]] {
  def insert(s: Session[F], st: Settlement, allocations: List[Allocation]): F[Unit]
  def find(id: SettlementId): F[Option[SettlementDetails]]
  def findForUpdate(s: Session[F], id: SettlementId): F[Option[SettlementDetails]]
  def list(kind: DocumentKind, from: LocalDate, to: LocalDate, limit: Int): F[List[Settlement]]
  def markVoided(s: Session[F], id: SettlementId, on: LocalDate): F[Unit]
}

object Settlements {

  import Codecs.{allocation as allocationC, documentKind as kindC, settlement as settlementC, settlementId as settlementIdC}
  import skunk.codec.all.{date, int4}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Settlements[F] =
    new Settlements[F] {

      def insert(s: Session[F], st: Settlement, allocations: List[Allocation]): F[Unit] =
        for {
          _ <- s.prepare(Q.insert).flatMap(_.execute(st))
          _ <- s.prepare(Q.insertAllocation).flatMap(pc => allocations.traverse_(a => pc.execute((st.id, a))))
        } yield ()

      def find(id: SettlementId): F[Option[SettlementDetails]] =
        pool.use(load(_, Q.byId, id))

      def findForUpdate(s: Session[F], id: SettlementId): F[Option[SettlementDetails]] =
        load(s, Q.byIdForUpdate, id)

      def list(kind: DocumentKind, from: LocalDate, to: LocalDate, limit: Int): F[List[Settlement]] =
        pool.use(_.prepare(Q.list).flatMap(_.stream((kind, from, to, limit), 64).compile.toList))

      def markVoided(s: Session[F], id: SettlementId, on: LocalDate): F[Unit] =
        s.prepare(Q.markVoided).flatMap(_.execute((on, id))).void

      private def load(s: Session[F], q: Query[SettlementId, Settlement], id: SettlementId): F[Option[SettlementDetails]] =
        s.prepare(q).flatMap(_.option(id)).flatMap {
          case None     => Concurrent[F].pure(None)
          case Some(st) => s.prepare(Q.allocations).flatMap(_.stream(id, 64).compile.toList).map(a => Some(SettlementDetails(st, a)))
        }
    }

  private object Q {

    private val cols =
      sql"""id, kind, number, contact_id, settle_date, currency, exchange_rate, account_id, reference, memo,
            total_minor, entry_id, fx_entry_id, voided_on, created_by, created_at"""

    private val allocationCols = sql"document_id, amount_minor, base_minor, document_base_minor"

    val insert: Command[Settlement] =
      sql"INSERT INTO settlements ($cols) VALUES ${settlementC.values}".command

    val insertAllocation: Command[(SettlementId, Allocation)] =
      sql"INSERT INTO settlement_allocations (settlement_id, $allocationCols) VALUES ($settlementIdC, $allocationC)".command

    val byId: Query[SettlementId, Settlement] =
      sql"SELECT $cols FROM settlements WHERE id = $settlementIdC".query(settlementC)

    val byIdForUpdate: Query[SettlementId, Settlement] =
      sql"SELECT $cols FROM settlements WHERE id = $settlementIdC FOR UPDATE".query(settlementC)

    val allocations: Query[SettlementId, Allocation] =
      sql"SELECT $allocationCols FROM settlement_allocations WHERE settlement_id = $settlementIdC".query(allocationC)

    val list: Query[(DocumentKind, LocalDate, LocalDate, Int), Settlement] =
      sql"""SELECT $cols FROM settlements
            WHERE kind = $kindC AND settle_date BETWEEN $date AND $date
            ORDER BY settle_date DESC, number DESC LIMIT $int4""".query(settlementC)

    val markVoided: Command[(LocalDate, SettlementId)] =
      sql"UPDATE settlements SET voided_on = $date WHERE id = $settlementIdC".command
  }
}
