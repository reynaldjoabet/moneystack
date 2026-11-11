package moneystack.service

import moneystack.domain.*
import moneystack.db.{Portfolios, Recommendations}
import moneystack.external.{Push, Telegram}

import cats.effect.*
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.util.UUID

/**
 * Publishes a recommendation, then notifies every investor whose portfolio is backed by its model: a push to each,
 * and a Telegram message to those who linked a chat. Notifying runs in the background, so publishing doesn't wait
 * for it, and a failed send is logged without affecting the publish or anyone else's notification.
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

  /** At most this many followers are notified at once. */
  val Parallelism = 8

  /** `notifications` owns the background sends; releasing it waits for them, so shutdown doesn't cut them off. */
  def make[F[_]: Async](
      recs: Recommendations[F],
      portfolios: Portfolios[F],
      telegram: Telegram[F],
      push: Push[F],
      notifications: Supervisor[F],
      logger: Logger[F]
  ): RecommendationService[F] = new RecommendationService[F] {

    private def attempt(what: String)(send: F[Unit]): F[Unit] =
      send.handleErrorWith(e => logger.warn(e)(s"could not $what"))

    private def notifyFollowers(r: Recommendation): F[Unit] = {
      val title = Title.assume(s"New ${r.action.toString.toLowerCase} signal: ${r.ticker.value}")
      val text  = s"${title.value}\n${r.rationale.value}"
      portfolios.followers(r.modelPortfolioId).flatMap { followers =>
        followers.parTraverseN(Parallelism) { case (user, chat) =>
          attempt(s"push recommendation ${r.id.value} to user ${user.value}")(push.send(user, title, r.rationale)) *>
            chat.traverse_(c => attempt(s"send recommendation ${r.id.value} to Telegram chat ${c.value}")(telegram.sendMessage(c, text)))
        }.void
      }.handleErrorWith(e => logger.error(e)(s"could not notify followers of recommendation ${r.id.value}"))
    }

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
        r = Recommendation(
          id = RecommendationId.assume(UUID.randomUUID()),
          modelPortfolioId = modelId,
          ticker = ticker,
          exchange = exchange,
          action = action,
          rationale = rationale,
          targetWeightBps = targetWeight,
          publishedAt = now,
          publishedBy = analystId
        )
        saved <- recs.publish(r)
        _     <- notifications.supervise(notifyFollowers(saved))
      } yield saved
  }
}
