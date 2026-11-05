package moneystack.auth

import moneystack.domain.PasswordHash

import cats.effect.*
import cats.syntax.all.*
import com.github.t3hnar.bcrypt.*

import java.nio.charset.StandardCharsets.UTF_8

/** bcrypt password hashing. Hashing takes ~250 ms of CPU, so it runs off the compute pool. */
object Passwords {

  val MinLength = 12

  /** bcrypt ignores everything past 72 bytes; longer passwords are refused rather than silently truncated. */
  val MaxBytes = 72

  private val Rounds = 12

  def validate(password: String): Either[String, String] =
    if (password.length < MinLength) Left(s"password must be at least $MinLength characters")
    else if (password.getBytes(UTF_8).length > MaxBytes) Left(s"password must be at most $MaxBytes bytes")
    else Right(password)

  def hash[F[_]: Sync](password: String): F[PasswordHash] =
    Sync[F].blocking(PasswordHash.assume(password.bcryptBounded(Rounds)))

  /**
   * Whether `password` matches `hash`. With no stored hash it still checks against a dummy one,
   * so an unknown email takes as long as a wrong password and can't be told apart by timing.
   */
  def verify[F[_]: Sync](password: String, hash: Option[PasswordHash]): F[Boolean] =
    Sync[F].blocking {
      val matches = password.isBcryptedSafeBounded(hash.fold(Dummy)(_.value)).getOrElse(false)
      matches && hash.isDefined
    }

  private lazy val Dummy: String = "dummy password".bcryptBounded(Rounds)

  /** Builds the dummy hash ahead of the first login, so that login isn't the slow one. */
  def warmUp[F[_]: Sync]: F[Unit] = Sync[F].blocking(Dummy).void
}
