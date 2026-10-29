package moneystack.portfolio.db

import moneystack.domain.*
import moneystack.portfolio.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Investment accounts and the balance snapshots of balance-tracked ones. Reads are scoped to an owner. */
trait Accounts[F[_]] {
  def create(a: InvestmentAccount): F[InvestmentAccount]
  def list(owner: UserId): F[List[InvestmentAccount]]
  def setActive(owner: UserId, id: InvestmentAccountId, active: Boolean): F[Option[InvestmentAccount]]
  def upsertSnapshot(s: BalanceSnapshot): F[Unit]
  def snapshots(ids: List[InvestmentAccountId]): F[List[BalanceSnapshot]]
}

object Accounts {

  import Codecs.{account as accountC, accountId as accountIdC, snapshot as snapshotC, userId as userIdC}
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Accounts[F] = new Accounts[F] {

    def create(a: InvestmentAccount): F[InvestmentAccount] =
      pool.use(_.prepare(Q.insert).flatMap(_.unique(a)))

    def list(owner: UserId): F[List[InvestmentAccount]] =
      pool.use(_.prepare(Q.list).flatMap(_.stream(owner, 64).compile.toList))

    def setActive(owner: UserId, id: InvestmentAccountId, active: Boolean): F[Option[InvestmentAccount]] =
      pool.use(_.prepare(Q.setActive).flatMap(_.option((active, id, owner))))

    def upsertSnapshot(s: BalanceSnapshot): F[Unit] =
      pool.use(_.prepare(Q.upsertSnapshot).flatMap(_.execute(s))).void

    def snapshots(ids: List[InvestmentAccountId]): F[List[BalanceSnapshot]] =
      if (ids.isEmpty) Concurrent[F].pure(Nil)
      else pool.use(_.prepare(Q.snapshots(ids.size)).flatMap(_.stream(ids, 256).compile.toList))
  }

  private object Q {

    private val cols = sql"id, owner_id, name, kind, tracking, currency, account_group, active, created_at"

    val insert: Query[InvestmentAccount, InvestmentAccount] =
      sql"INSERT INTO portfolio.accounts ($cols) VALUES ${accountC.values} RETURNING $cols".query(accountC)

    val list: Query[UserId, InvestmentAccount] =
      sql"SELECT $cols FROM portfolio.accounts WHERE owner_id = $userIdC ORDER BY name".query(accountC)

    val setActive: Query[(Boolean, InvestmentAccountId, UserId), InvestmentAccount] =
      sql"UPDATE portfolio.accounts SET active = $bool WHERE id = $accountIdC AND owner_id = $userIdC RETURNING $cols".query(accountC)

    val upsertSnapshot: Command[BalanceSnapshot] =
      sql"""INSERT INTO portfolio.balance_snapshots (account_id, snapshot_date, balance, invested) VALUES ${snapshotC.values}
            ON CONFLICT (account_id, snapshot_date) DO UPDATE SET balance = EXCLUDED.balance, invested = EXCLUDED.invested""".command

    def snapshots(n: Int): Query[List[InvestmentAccountId], BalanceSnapshot] =
      sql"""SELECT account_id, snapshot_date, balance, invested FROM portfolio.balance_snapshots
            WHERE account_id IN (${accountIdC.list(n)}) ORDER BY snapshot_date""".query(snapshotC)
  }
}
