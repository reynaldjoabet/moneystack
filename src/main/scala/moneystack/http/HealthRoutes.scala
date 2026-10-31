package moneystack.http

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import skunk.Session
import skunk.codec.all.int4
import skunk.implicits.*

import scala.concurrent.duration.*

/**
 * Probes for orchestrators. Liveness only says the process answers; readiness
 * also needs Postgres to answer within `timeout`, and is 503 otherwise.
 */
final class HealthRoutes[F[_]: Temporal](pool: Resource[F, Session[F]], timeout: FiniteDuration = 2.seconds) extends Http4sDsl[F] {

  private val databaseUp: F[Boolean] =
    pool.use(_.unique(sql"SELECT 1".query(int4))).timeout(timeout).attempt.map(_.isRight)

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root / "health" | GET -> Root / "health" / "live" =>
      Ok(Map("status" -> "ok").asJson)

    case GET -> Root / "health" / "ready" =>
      databaseUp.flatMap {
        case true  => Ok(Map("status" -> "ok", "database" -> "up").asJson)
        case false => ServiceUnavailable(Map("status" -> "unavailable", "database" -> "down").asJson)
      }
  }
}
