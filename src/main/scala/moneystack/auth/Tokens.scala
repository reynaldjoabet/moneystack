package moneystack.auth

import moneystack.domain.UserId

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.parser.parse
import pdi.jwt.{JwtAlgorithm, JwtCirce, JwtClaim}

import java.time.{Clock as JClock, Instant, ZoneOffset}
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/**
 * Signed access tokens (HS256 JWTs) naming the user they were issued to, and the user's token version at the
 * time: bumping the version in the database revokes every token issued before.
 */
trait Tokens[F[_]] {
  def issue(user: UserId, version: Int): F[Tokens.Issued]

  /** Who a token was issued to, if it is well formed, signed with our key, ours, and not expired. */
  def verify(token: String): F[Option[Tokens.Claims]]
}

object Tokens {

  final case class Issued(token: String, expiresAt: Instant)

  final case class Claims(user: UserId, version: Int)

  /** Below this, an HMAC-SHA256 key is weaker than the hash (RFC 7518 §3.2). */
  val MinSecretBytes = 32

  def make[F[_]: Sync](secret: String, issuer: String, ttl: FiniteDuration): Tokens[F] =
    new Tokens[F] {

      def issue(user: UserId, version: Int): F[Issued] =
        Clock[F].realTimeInstant.map { now =>
          val expiresAt = now.plusSeconds(ttl.toSeconds)
          val claim     = JwtClaim(
            content = Json.obj("ver" -> Json.fromInt(version)).noSpaces,
            issuer = Some(issuer),
            subject = Some(user.value.toString),
            issuedAt = Some(now.getEpochSecond),
            expiration = Some(expiresAt.getEpochSecond),
            jwtId = Some(UUID.randomUUID().toString)
          )
          Issued(JwtCirce.encode(claim, secret, JwtAlgorithm.HS256), expiresAt)
        }

      // Only HS256 is accepted, so a token can't pick its own algorithm ("none", or RS256 keyed with our secret).
      def verify(token: String): F[Option[Claims]] =
        Clock[F].realTimeInstant.map { now =>
          JwtCirce(JClock.fixed(now, ZoneOffset.UTC))
            .decode(token, secret, Seq(JwtAlgorithm.HS256))
            .toOption
            .filter(_.issuer.contains(issuer))
            .flatMap { claim =>
              (
                claim.subject.flatMap(s => Either.catchNonFatal(UUID.fromString(s)).toOption).map(UserId.assume),
                parse(claim.content).flatMap(_.hcursor.get[Int]("ver")).toOption
              ).mapN(Claims.apply)
            }
        }
    }
}
