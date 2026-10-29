package wealth.budget.db

import wealth.domain.*
import wealth.budget.domain.*
import wealth.platform.FxQuote

import cats.effect.*
import cats.syntax.all.*
import skunk.{Command, Query, Session}
import skunk.implicits.*

/** Categories, payees, tags and exchange rates. */
trait Catalog[F[_]] {
  def createCategory(c: Category): F[Category]
  /** Inserts unless the owner has a category of that kind and name. */
  def ensureCategory(c: Category): F[Unit]
  def categories(owner: UserId): F[List[Category]]
  def setCategoryArchived(owner: UserId, id: CategoryId, archived: Boolean): F[Option[Category]]

  def createPayee(p: Payee): F[Payee]
  def payees(owner: UserId): F[List[Payee]]
  def updatePayee(p: Payee): F[Option[Payee]]

  def createTag(t: Tag): F[Tag]
  def tags(owner: UserId): F[List[Tag]]

  def upsertRates(rs: List[FxQuote]): F[Unit]
  def rates(): F[List[FxQuote]]
}

object Catalog {

  import Codecs.{category as categoryC, categoryId as categoryIdC, fxQuote as fxC, payee as payeeC, payeeId as payeeIdC,
                 tag as tagC, userId as userIdC}
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Catalog[F] = new Catalog[F] {

    def createCategory(c: Category): F[Category] = pool.use(_.prepare(Q.insertCategory).flatMap(_.unique(c)))

    def ensureCategory(c: Category): F[Unit] = pool.use(_.prepare(Q.ensureCategory).flatMap(_.execute(c))).void

    def categories(owner: UserId): F[List[Category]] = pool.use(_.prepare(Q.categories).flatMap(_.stream(owner, 128).compile.toList))

    def setCategoryArchived(owner: UserId, id: CategoryId, archived: Boolean): F[Option[Category]] =
      pool.use(_.prepare(Q.archiveCategory).flatMap(_.option((archived, id, owner))))

    def createPayee(p: Payee): F[Payee] = pool.use(_.prepare(Q.insertPayee).flatMap(_.unique(p)))

    def payees(owner: UserId): F[List[Payee]] = pool.use(_.prepare(Q.payees).flatMap(_.stream(owner, 128).compile.toList))

    def updatePayee(p: Payee): F[Option[Payee]] = pool.use(_.prepare(Q.updatePayee).flatMap(_.option(p)))

    def createTag(t: Tag): F[Tag] = pool.use(_.prepare(Q.insertTag).flatMap(_.unique(t)))

    def tags(owner: UserId): F[List[Tag]] = pool.use(_.prepare(Q.tags).flatMap(_.stream(owner, 128).compile.toList))

    def upsertRates(rs: List[FxQuote]): F[Unit] =
      pool.use(s => s.transaction.use(_ => s.prepare(Q.upsertRate).flatMap(pc => rs.traverse_(pc.execute))))

    def rates(): F[List[FxQuote]] = pool.use(_.prepare(Q.rates).flatMap(_.stream(skunk.Void, 512).compile.toList))
  }

  private object Q {

    private val catCols = sql"id, owner_id, name, kind, parent_id, colour, archived"

    val insertCategory: Query[Category, Category] =
      sql"INSERT INTO budget.categories ($catCols) VALUES ${categoryC.values} RETURNING $catCols".query(categoryC)

    val ensureCategory: Command[Category] =
      sql"INSERT INTO budget.categories ($catCols) VALUES ${categoryC.values} ON CONFLICT (owner_id, kind, name) DO NOTHING".command

    val categories: Query[UserId, Category] =
      sql"SELECT $catCols FROM budget.categories WHERE owner_id = $userIdC ORDER BY kind, name".query(categoryC)

    val archiveCategory: Query[(Boolean, CategoryId, UserId), Category] =
      sql"UPDATE budget.categories SET archived = $bool WHERE id = $categoryIdC AND owner_id = $userIdC RETURNING $catCols".query(categoryC)

    private val payeeCols = sql"id, owner_id, name, aliases, default_category_id"

    val insertPayee: Query[Payee, Payee] =
      sql"INSERT INTO budget.payees ($payeeCols) VALUES ${payeeC.values} RETURNING $payeeCols".query(payeeC)

    val payees: Query[UserId, Payee] =
      sql"SELECT $payeeCols FROM budget.payees WHERE owner_id = $userIdC ORDER BY name".query(payeeC)

    val updatePayee: Query[Payee, Payee] =
      sql"""UPDATE budget.payees SET ($payeeCols) = (${payeeC}) WHERE id = $payeeIdC AND owner_id = $userIdC
            RETURNING $payeeCols""".query(payeeC).contramap[Payee](p => (p, p.id, p.ownerId))

    val insertTag: Query[Tag, Tag] =
      sql"INSERT INTO budget.tags (id, owner_id, name) VALUES ${tagC.values} RETURNING id, owner_id, name".query(tagC)

    val tags: Query[UserId, Tag] =
      sql"SELECT id, owner_id, name FROM budget.tags WHERE owner_id = $userIdC ORDER BY name".query(tagC)

    val upsertRate: Command[FxQuote] =
      sql"""INSERT INTO budget.fx_rates (from_ccy, to_ccy, rate_date, rate) VALUES ${fxC.values}
            ON CONFLICT (from_ccy, to_ccy, rate_date) DO UPDATE SET rate = EXCLUDED.rate""".command

    val rates: Query[skunk.Void, FxQuote] =
      sql"SELECT from_ccy, to_ccy, rate_date, rate FROM budget.fx_rates".query(fxC)
  }
}
