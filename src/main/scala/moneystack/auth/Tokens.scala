package moneystack.auth

import moneystack.domain.UserId

import cats.effect.*
import cats.syntax.all.*
import pdi.jwt.{JwtAlgorithm, JwtCirce, JwtClaim}

import java.time.{Clock as JClock, Instant, ZoneOffset}
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/** Signed access tokens (HS256 JWTs) naming the user they were issued to. */
trait Tokens[F[_]] {
  def issue(user: UserId): F[Tokens.Issued]

  /** The user a token names, if it is well formed, signed with our key, ours, and not expired. */
  def verify(token: String): F[Option[UserId]]
}

object Tokens {

  final case class Issued(token: String, expiresAt: Instant)

  /** Below this, an HMAC-SHA256 key is weaker than the hash (RFC 7518 §3.2). */
  val MinSecretBytes = 32

  def make[F[_]: Sync](secret: String, issuer: String, ttl: FiniteDuration): Tokens[F] =
    new Tokens[F] {

      def issue(user: UserId): F[Issued] =
        Clock[F].realTimeInstant.map { now =>
          val expiresAt = now.plusSeconds(ttl.toSeconds)
          val claim     = JwtClaim(
            issuer = Some(issuer),
            subject = Some(user.value.toString),
            issuedAt = Some(now.getEpochSecond),
            expiration = Some(expiresAt.getEpochSecond),
            jwtId = Some(UUID.randomUUID().toString)
          )
          Issued(JwtCirce.encode(claim, secret, JwtAlgorithm.HS256), expiresAt)
        }

      // Only HS256 is accepted, so a token can't pick its own algorithm ("none", or RS256 keyed with our secret).
      def verify(token: String): F[Option[UserId]] =
        Clock[F].realTimeInstant.map { now =>
          JwtCirce(JClock.fixed(now, ZoneOffset.UTC))
            .decode(token, secret, Seq(JwtAlgorithm.HS256))
            .toOption
            .filter(_.issuer.contains(issuer))
            .flatMap(_.subject)
            .flatMap(s => Either.catchNonFatal(UUID.fromString(s)).toOption)
            .map(UserId.assume)
        }
    }
}
