package wealth.db

import wealth.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait Portfolios[F[_]] {
  // ---- model portfolios ----
  def createModel(mp: ModelPortfolio, alloc: List[ModelAllocation]): F[ModelPortfolio]
  def listModels(): F[List[ModelPortfolio]]
  def modelAllocations(id: ModelPortfolioId): F[List[ModelAllocation]]
  def replaceAllocations(id: ModelPortfolioId, alloc: List[ModelAllocation]): F[Unit]

  // ---- investor portfolios ----
  def createPortfolio(p: Portfolio): F[Portfolio]
  def listFor(userId: UserId): F[List[Portfolio]]
  def find(id: PortfolioId): F[Option[Portfolio]]
}

object Portfolios {

  import Codecs.{modelPortfolio as modelC, modelAllocation as allocC,
                 modelPortfolioId as modelIdC, portfolio as portfolioC,
                 portfolioId as portfolioIdC, userId as userIdC}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Portfolios[F] =
    new Portfolios[F] {

      def createModel(mp: ModelPortfolio, alloc: List[ModelAllocation]): F[ModelPortfolio] =
        pool.use { s =>
          s.transaction.use { _ =>
            for {
              saved <- s.prepare(Q.insertModel).flatMap(_.unique(mp))
              _     <- s.prepare(Q.insertAlloc).flatMap(pc => alloc.traverse_(pc.execute))
            } yield saved
          }
        }

      def listModels(): F[List[ModelPortfolio]] =
        pool.use(_.prepare(Q.listModels).flatMap(_.stream(skunk.Void, 32).compile.toList))

      def modelAllocations(id: ModelPortfolioId): F[List[ModelAllocation]] =
        pool.use(_.prepare(Q.allocsFor).flatMap(_.stream(id, 64).compile.toList))

      def replaceAllocations(id: ModelPortfolioId, alloc: List[ModelAllocation]): F[Unit] =
        pool.use { s =>
          s.transaction.use { _ =>
            for {
              _ <- s.prepare(Q.deleteAllocs).flatMap(_.execute(id))
              _ <- s.prepare(Q.insertAlloc).flatMap(pc => alloc.traverse_(pc.execute))
            } yield ()
          }
        }

      def createPortfolio(p: Portfolio): F[Portfolio] =
        pool.use(_.prepare(Q.insertPortfolio).flatMap(_.unique(p)))

      def listFor(userId: UserId): F[List[Portfolio]] =
        pool.use(_.prepare(Q.listFor).flatMap(_.stream(userId, 16).compile.toList))

      def find(id: PortfolioId): F[Option[Portfolio]] =
        pool.use(_.prepare(Q.byId).flatMap(_.option(id)))
    }

  private object Q {

    import skunk.codec.all.timestamptz

    val insertModel: Query[ModelPortfolio, ModelPortfolio] =
      sql"""INSERT INTO model_portfolios (id, name, strategy, created_by, description, created_at, updated_at)
            VALUES (${Codecs.modelPortfolioId}, ${Codecs.title}, ${Codecs.portfolioStrategy},
                    ${Codecs.userId}, ${Codecs.body.opt}, $timestamptz, $timestamptz)
            RETURNING id, name, strategy, created_by, created_at, updated_at, description""".query(modelC)
        .contramap[ModelPortfolio](m =>
          (m.id, m.name, m.strategy, m.createdBy, m.description,
           m.createdAt.atOffset(java.time.ZoneOffset.UTC),
           m.updatedAt.atOffset(java.time.ZoneOffset.UTC))
        )

    val insertAlloc: Command[ModelAllocation] =
      sql"""INSERT INTO model_allocations (model_portfolio_id, ticker, exchange, weight_bps)
            VALUES ${allocC.values}
            ON CONFLICT (model_portfolio_id, ticker, exchange) DO UPDATE
              SET weight_bps = EXCLUDED.weight_bps""".command

    val deleteAllocs: Command[ModelPortfolioId] =
      sql"DELETE FROM model_allocations WHERE model_portfolio_id = $modelIdC".command

    val allocsFor: Query[ModelPortfolioId, ModelAllocation] =
      sql"""SELECT model_portfolio_id, ticker, exchange, weight_bps
            FROM model_allocations WHERE model_portfolio_id = $modelIdC
            ORDER BY weight_bps DESC""".query(allocC)

    val listModels: Query[skunk.Void, ModelPortfolio] =
      sql"""SELECT id, name, strategy, created_by, created_at, updated_at, description
            FROM model_portfolios ORDER BY updated_at DESC""".query(modelC)

    val insertPortfolio: Query[Portfolio, Portfolio] =
      sql"""INSERT INTO portfolios (id, user_id, name, base_currency, backed_by, created_at)
            VALUES ${portfolioC.values}
            RETURNING id, user_id, name, base_currency, backed_by, created_at""".query(portfolioC)

    val listFor: Query[UserId, Portfolio] =
      sql"""SELECT id, user_id, name, base_currency, backed_by, created_at
            FROM portfolios WHERE user_id = $userIdC ORDER BY created_at""".query(portfolioC)

    val byId: Query[PortfolioId, Portfolio] =
      sql"""SELECT id, user_id, name, base_currency, backed_by, created_at
            FROM portfolios WHERE id = $portfolioIdC""".query(portfolioC)
  }
}
