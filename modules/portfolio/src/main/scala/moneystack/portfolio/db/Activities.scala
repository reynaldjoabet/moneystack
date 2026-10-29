package moneystack.portfolio.db

import moneystack.portfolio.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Activities are user records: editable and deletable, unlike ledger entries. */
trait Activities[F[_]] {
  def insert(a: Activity): F[Activity]
  /** Inserts rows whose `(account, import key)` is new; returns those inserted. */
  def insertImported(as: List[Activity]): F[List[Activity]]
  def replace(a: Activity): F[Option[Activity]]
  def delete(id: ActivityId, account: InvestmentAccountId): F[Boolean]
  def forAccounts(ids: List[InvestmentAccountId]): F[List[Activity]]
}

object Activities {

  import Codecs.{accountId as accountIdC, activity as activityC, activityId as activityIdC}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Activities[F] = new Activities[F] {

    def insert(a: Activity): F[Activity] = pool.use(_.prepare(Q.insert).flatMap(_.unique(a)))

    def insertImported(as: List[Activity]): F[List[Activity]] =
      pool.use(s => s.transaction.use(_ => s.prepare(Q.insertIfNew).flatMap(pq => as.flatTraverse(a => pq.option(a).map(_.toList)))))

    def replace(a: Activity): F[Option[Activity]] = pool.use(_.prepare(Q.replace).flatMap(_.option(a)))

    def delete(id: ActivityId, account: InvestmentAccountId): F[Boolean] =
      pool.use(_.prepare(Q.delete).flatMap(_.execute((id, account)))).map {
        case skunk.data.Completion.Delete(n) => n > 0
        case _                               => false
      }

    def forAccounts(ids: List[InvestmentAccountId]): F[List[Activity]] =
      if (ids.isEmpty) Concurrent[F].pure(Nil)
      else pool.use(_.prepare(Q.forAccounts(ids.size)).flatMap(_.stream(ids, 512).compile.toList))
  }

  private object Q {

    private val cols =
      sql"""id, account_id, asset_id, kind, activity_date, quantity, unit_price, amount, fee, currency, external,
            counter_account_id, note, import_key, created_at"""

    val insert: Query[Activity, Activity] =
      sql"INSERT INTO portfolio.activities ($cols) VALUES ${activityC.values} RETURNING $cols".query(activityC)

    val insertIfNew: Query[Activity, Activity] =
      sql"""INSERT INTO portfolio.activities ($cols) VALUES ${activityC.values}
            ON CONFLICT (account_id, import_key) DO NOTHING RETURNING $cols""".query(activityC)

    val replace: Query[Activity, Activity] =
      sql"""UPDATE portfolio.activities SET ($cols) = (${activityC})
            WHERE id = $activityIdC AND account_id = $accountIdC RETURNING $cols""".query(activityC)
        .contramap[Activity](a => (a, a.id, a.accountId))

    val delete: Command[(ActivityId, InvestmentAccountId)] =
      sql"DELETE FROM portfolio.activities WHERE id = $activityIdC AND account_id = $accountIdC".command

    def forAccounts(n: Int): Query[List[InvestmentAccountId], Activity] =
      sql"""SELECT $cols FROM portfolio.activities WHERE account_id IN (${accountIdC.list(n)})
            ORDER BY activity_date, created_at, id""".query(activityC)
  }
}
