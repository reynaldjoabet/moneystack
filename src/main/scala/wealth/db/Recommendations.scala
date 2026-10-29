package wealth.db

import wealth.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait Recommendations[F[_]] {
  def publish(r: Recommendation): F[Recommendation]
  def listForModel(modelId: ModelPortfolioId, limit: Int): F[List[Recommendation]]
  def listLatest(limit: Int): F[List[Recommendation]]
}

object Recommendations {

  import Codecs.{recommendation as recC, modelPortfolioId as modelIdC}
  import skunk.codec.all.int4

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Recommendations[F] =
    new Recommendations[F] {
      def publish(r: Recommendation): F[Recommendation] =
        pool.use(_.prepare(Q.insert).flatMap(_.unique(r)))
      def listForModel(modelId: ModelPortfolioId, limit: Int): F[List[Recommendation]] =
        pool.use(_.prepare(Q.byModel).flatMap(_.stream((modelId, limit), 64).compile.toList))
      def listLatest(limit: Int): F[List[Recommendation]] =
        pool.use(_.prepare(Q.latest).flatMap(_.stream(limit, 64).compile.toList))
    }

  private object Q {
    val insert: Query[Recommendation, Recommendation] =
      sql"""INSERT INTO recommendations (id, model_portfolio_id, ticker, exchange, action, rationale,
                                         target_weight_bps, published_at, published_by)
            VALUES $recC
            RETURNING id, model_portfolio_id, ticker, exchange, action, rationale,
                      target_weight_bps, published_at, published_by""".query(recC)

    val byModel: Query[(ModelPortfolioId, Int), Recommendation] =
      sql"""SELECT id, model_portfolio_id, ticker, exchange, action, rationale,
                   target_weight_bps, published_at, published_by
            FROM recommendations WHERE model_portfolio_id = $modelIdC
            ORDER BY published_at DESC LIMIT $int4""".query(recC)

    val latest: Query[Int, Recommendation] =
      sql"""SELECT id, model_portfolio_id, ticker, exchange, action, rationale,
                   target_weight_bps, published_at, published_by
            FROM recommendations ORDER BY published_at DESC LIMIT $int4""".query(recC)
  }
}
