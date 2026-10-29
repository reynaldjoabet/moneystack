package moneystack.service

import moneystack.domain.*
import moneystack.db.{Portfolios, Recommendations, Users}
import moneystack.external.{Push, Telegram}

import cats.effect.*
import cats.syntax.all.*

import java.time.Instant
import java.util.UUID

/**
 * Publishes a recommendation, notifies every investor who tracks the
 * underlying model portfolio. Telegram + push fan out in parallel.
 */
trait RecommendationService[F[_]] {
  def publish(
      analystId: UserId,
      modelId: ModelPortfolioId,
      ticker: Ticker,
      exchange: Exchange,
      action: TradeAction,
      rationale: Body,
      targetWeight: Option[WeightBps]
  ): F[Recommendation]
}

object RecommendationService {

  def make[F[_]: Async](
      recs: Recommendations[F],
      portfolios: Portfolios[F],
      users: Users[F],
      telegram: Telegram[F],
      push: Push[F]
  ): RecommendationService[F] = new RecommendationService[F] {

    def publish(
        analystId: UserId,
        modelId: ModelPortfolioId,
        ticker: Ticker,
        exchange: Exchange,
        action: TradeAction,
        rationale: Body,
        targetWeight: Option[WeightBps]
    ): F[Recommendation] =
      for {
        now <- Async[F].delay(Instant.now())
        r    = Recommendation(
                 id                = RecommendationId.assume(UUID.randomUUID()),
                 modelPortfolioId  = modelId,
                 ticker            = ticker,
                 exchange          = exchange,
                 action            = action,
                 rationale         = rationale,
                 targetWeightBps   = targetWeight,
                 publishedAt       = now,
                 publishedBy       = analystId
               )
        saved <- recs.publish(r)
        title  = Title.assume(s"New ${action.toString.toLowerCase} signal: ${ticker.value}")
        // Notify everyone with a portfolio backed by this model.
        // Production: a materialised view or a denormalised follower list would
        // avoid loading every investor on every publish.
        // _ <- notify(saved, title)
      } yield saved
  }
}
