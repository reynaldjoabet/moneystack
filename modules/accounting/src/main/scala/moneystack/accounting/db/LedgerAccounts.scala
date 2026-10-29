package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Chart of accounts. */
trait LedgerAccounts[F[_]] {
  def create(a: Account): F[Account]

  /** Inserts unless an account with the same id or code exists. */
  def ensure(a: Account): F[Unit]
  def list(): F[List[Account]]
  def setActive(id: AccountId, active: Boolean): F[Option[Account]]
}

object LedgerAccounts {

  import Codecs.{account as accountC, accountId as accountIdC}
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): LedgerAccounts[F] =
    new LedgerAccounts[F] {

      def create(a: Account): F[Account] =
        pool.use(_.prepare(Q.insert).flatMap(_.unique(a)))

      def ensure(a: Account): F[Unit] =
        pool.use(_.prepare(Q.insertIfAbsent).flatMap(_.execute(a))).void

      def list(): F[List[Account]] =
        pool.use(_.prepare(Q.list).flatMap(_.stream(skunk.Void, 128).compile.toList))

      def setActive(id: AccountId, active: Boolean): F[Option[Account]] =
        pool.use(_.prepare(Q.setActive).flatMap(_.option((active, id))))
    }

  private object Q {

    val insert: Query[Account, Account] =
      sql"""INSERT INTO ledger_accounts (id, code, name, account_type, currency, parent_id, active, created_at)
            VALUES ${accountC.values}
            RETURNING id, code, name, account_type, currency, parent_id, active, created_at""".query(accountC)

    val insertIfAbsent: Command[Account] =
      sql"""INSERT INTO ledger_accounts (id, code, name, account_type, currency, parent_id, active, created_at)
            VALUES ${accountC.values}
            ON CONFLICT DO NOTHING""".command

    val list: Query[skunk.Void, Account] =
      sql"""SELECT id, code, name, account_type, currency, parent_id, active, created_at
            FROM ledger_accounts ORDER BY code""".query(accountC)

    val setActive: Query[(Boolean, AccountId), Account] =
      sql"""UPDATE ledger_accounts SET active = $bool WHERE id = $accountIdC
            RETURNING id, code, name, account_type, currency, parent_id, active, created_at""".query(accountC)
  }
}
