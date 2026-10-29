package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Customers and vendors. */
trait Contacts[F[_]] {
  def create(c: Contact): F[Contact]

  /** Inserts unless a contact with the same id exists. */
  def ensure(c: Contact): F[Unit]
  def list(kind: Option[ContactKind]): F[List[Contact]]
  def find(id: ContactId): F[Option[Contact]]
  def setActive(id: ContactId, active: Boolean): F[Option[Contact]]
}

object Contacts {

  import Codecs.{contact as contactC, contactId as contactIdC, contactKind as kindC}
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Contacts[F] =
    new Contacts[F] {

      def create(c: Contact): F[Contact] =
        pool.use(_.prepare(Q.insert).flatMap(_.unique(c)))

      def ensure(c: Contact): F[Unit] =
        pool.use(_.prepare(Q.insertIfAbsent).flatMap(_.execute(c))).void

      def list(kind: Option[ContactKind]): F[List[Contact]] =
        pool.use(_.prepare(Q.list).flatMap(_.stream(kind, 128).compile.toList))

      def find(id: ContactId): F[Option[Contact]] =
        pool.use(_.prepare(Q.byId).flatMap(_.option(id)))

      def setActive(id: ContactId, active: Boolean): F[Option[Contact]] =
        pool.use(_.prepare(Q.setActive).flatMap(_.option((active, id))))
    }

  private object Q {

    private val cols = sql"id, kind, name, email, currency, user_id, active, created_at"

    val insert: Query[Contact, Contact] =
      sql"INSERT INTO contacts ($cols) VALUES ${contactC.values} RETURNING $cols".query(contactC)

    val insertIfAbsent: Command[Contact] =
      sql"INSERT INTO contacts ($cols) VALUES ${contactC.values} ON CONFLICT DO NOTHING".command

    val list: Query[Option[ContactKind], Contact] =
      sql"""SELECT $cols FROM contacts WHERE (${kindC.opt}::varchar IS NULL OR kind = ${kindC.opt})
            ORDER BY name""".query(contactC).contramap[Option[ContactKind]](k => (k, k))

    val byId: Query[ContactId, Contact] =
      sql"SELECT $cols FROM contacts WHERE id = $contactIdC".query(contactC)

    val setActive: Query[(Boolean, ContactId), Contact] =
      sql"UPDATE contacts SET active = $bool WHERE id = $contactIdC RETURNING $cols".query(contactC)
  }
}
