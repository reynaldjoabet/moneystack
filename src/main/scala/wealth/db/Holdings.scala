package wealth.db

import wealth.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait Holdings[F[_]] {
  def upsert(h: Holding): F[Holding]
  def listFor(portfolioId: PortfolioId): F[List[Holding]]
  def delete(id: HoldingId): F[Unit]
}

object Holdings {

  import Codecs.{holding as holdingC, holdingId as holdingIdC, portfolioId as portfolioIdC}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Holdings[F] =
    new Holdings[F] {
      def upsert(h: Holding): F[Holding] =
        pool.use(_.prepare(Q.upsert).flatMap(_.unique(h)))
      def listFor(pid: PortfolioId): F[List[Holding]] =
        pool.use(_.prepare(Q.listFor).flatMap(_.stream(pid, 128).compile.toList))
      def delete(id: HoldingId): F[Unit] =
        pool.use(_.prepare(Q.del).flatMap(_.execute(id))).void
    }

  private object Q {

    val upsert: Query[Holding, Holding] =
      sql"""INSERT INTO holdings (id, portfolio_id, ticker, exchange, quantity, avg_cost_unit, updated_at)
            VALUES $holdingC
            ON CONFLICT (portfolio_id, ticker, exchange) DO UPDATE
              SET quantity = EXCLUDED.quantity,
                  avg_cost_unit = EXCLUDED.avg_cost_unit,
                  updated_at = EXCLUDED.updated_at
            RETURNING id, portfolio_id, ticker, exchange, quantity, avg_cost_unit, updated_at""".query(holdingC)

    val listFor: Query[PortfolioId, Holding] =
      sql"""SELECT id, portfolio_id, ticker, exchange, quantity, avg_cost_unit, updated_at
            FROM holdings WHERE portfolio_id = $portfolioIdC ORDER BY ticker""".query(holdingC)

    val del: Command[HoldingId] =
      sql"DELETE FROM holdings WHERE id = $holdingIdC".command
  }
}
