package moneystack.service

import moneystack.auth.Passwords
import moneystack.db.{PasswordResets, Users}
import moneystack.domain.*
import moneystack.external.Mailer

import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

import java.nio.charset.StandardCharsets.UTF_8
import java.security.{MessageDigest, SecureRandom}
import java.util.{Base64, HexFormat}
import scala.concurrent.duration.FiniteDuration

/** "Forgot my password": a one-time token is emailed to the account, and whoever presents it sets a new password. */
trait PasswordResetService[F[_]] {

  /**
   * Emails a reset token if `email` has an account. It answers at once and the same way either way, working in the
   * background, so neither the response nor its timing reveals whether the account exists.
   */
  def request(email: Email): F[Unit]

  /** Sets the password and revokes every token the user holds. `Left` says what was wrong, for the client. */
  def reset(token: String, newPassword: String): F[Either[String, User]]
}

object PasswordResetService {

  private val random = new SecureRandom()

  /** 256 random bits, URL-safe. */
  private def newToken[F[_]: Sync]: F[String] = Sync[F].delay {
    val bytes = new Array[Byte](32)
    random.nextBytes(bytes)
    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
  }

  /** Tokens are long and random, so a plain hash is enough: there is nothing to brute-force, unlike a password. */
  def hashOf(token: String): String =
    HexFormat.of.formatHex(MessageDigest.getInstance("SHA-256").digest(token.getBytes(UTF_8)))

  def make[F[_]: Async](
      users: Users[F],
      resets: PasswordResets[F],
      mailer: Mailer[F],
      ttl: FiniteDuration,
      background: Supervisor[F],
      logger: Logger[F]
  ): PasswordResetService[F] = new PasswordResetService[F] {

    private def send(email: Email): F[Unit] =
      users.findByEmail(email).flatMap(_.traverse_ { u =>
        for {
          token <- newToken[F]
          now   <- Async[F].realTimeInstant
          expires = now.plusMillis(ttl.toMillis)
          _ <- resets.create(u.id, hashOf(token), expires, now)
          _ <- mailer.sendPasswordReset(u.email, token, expires)
        } yield ()
      })

    def request(email: Email): F[Unit] =
      background.supervise(send(email).handleErrorWith(e => logger.error(e)("could not send a password reset email"))).void

    def reset(token: String, newPassword: String): F[Either[String, User]] =
      Passwords.validate(newPassword) match {
        case Left(err) => Async[F].pure(Left(err))
        case Right(pw) =>
          Async[F].realTimeInstant.flatMap(resets.consume(hashOf(token), _)).flatMap {
            case None       => Async[F].pure(Left("the reset link is invalid or has expired"))
            case Some(user) =>
              Passwords.hash[F](pw).flatMap(users.setPassword(user, _)).map(_.toRight("the reset link is invalid or has expired"))
          }
      }
  }
}
