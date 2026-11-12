package moneystack.db

import moneystack.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.codec.all.{bpchar, timestamptz}
import skunk.implicits.*

import java.time.{Instant, ZoneOffset}

/** One-time password reset tokens, stored as hashes. */
trait PasswordResets[F[_]] {

  /** Records a token for `user`, replacing any they already had, and drops everyone's expired ones. */
  def create(user: UserId, tokenHash: String, expiresAt: Instant, now: Instant): F[Unit]

  /** Marks the token used and answers its user, unless it is unknown, expired or already used. */
  def consume(tokenHash: String, now: Instant): F[Option[UserId]]
}

object PasswordResets {

  private val at = timestamptz.contramap[Instant](_.atOffset(ZoneOffset.UTC))

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): PasswordResets[F] = new PasswordResets[F] {

    def create(user: UserId, tokenHash: String, expiresAt: Instant, now: Instant): F[Unit] =
      pool.use(s =>
        s.transaction.use { _ =>
          s.prepare(Q.clear).flatMap(_.execute((user, now))) *>
            s.prepare(Q.insert).flatMap(_.execute((tokenHash, user, expiresAt))).void
        }
      )

    def consume(tokenHash: String, now: Instant): F[Option[UserId]] =
      pool.use(_.prepare(Q.consume).flatMap(_.option((now, tokenHash, now))))
  }

  private object Q {

    val clear: Command[(UserId, Instant)] =
      sql"DELETE FROM password_resets WHERE user_id = ${Codecs.userId} OR expires_at < $at".command

    val insert: Command[(String, UserId, Instant)] =
      sql"INSERT INTO password_resets (token_hash, user_id, expires_at) VALUES (${bpchar(64)}, ${Codecs.userId}, $at)".command

    // One statement, so two requests with the same token can't both succeed.
    val consume: Query[(Instant, String, Instant), UserId] =
      sql"""UPDATE password_resets SET used_at = $at
            WHERE token_hash = ${bpchar(64)} AND used_at IS NULL AND expires_at > $at
            RETURNING user_id""".query(Codecs.userId)
  }
}
