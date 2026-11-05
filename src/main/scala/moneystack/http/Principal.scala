package moneystack.http

import moneystack.auth.Tokens
import moneystack.domain.*
import moneystack.db.Users
import moneystack.platform.{Auth, AuthFailure}

import cats.effect.*
import cats.syntax.all.*
import org.http4s.{AuthScheme, Credentials, Request}
import org.http4s.headers.Authorization

/**
 * Resolves the caller from an `Authorization: Bearer <token>` header. The user is looked up on every
 * request, so a deleted user or a changed role takes effect at once rather than when the token expires.
 */
final class Principal[F[_]: Sync](tokens: Tokens[F], users: Users[F]) {

  /** The signed-in user, or `Unauthenticated` without a valid token for a user who still exists. */
  def user(req: Request[F]): F[Either[AuthFailure, User]] =
    Principal.bearer(req) match {
      case None        => Sync[F].pure(Left(AuthFailure.Unauthenticated))
      case Some(token) =>
        tokens.verify(token).flatMap {
          case None      => Sync[F].pure(Left(AuthFailure.Unauthenticated))
          case Some(uid) => users.find(uid).map(_.toRight(AuthFailure.Unauthenticated))
        }
    }

  /** Callers who hold `role`. */
  def withRole(role: Role): Auth[F] =
    req => user(req).map(_.flatMap(u => if (u.role == role) Right(u.id) else Left(AuthFailure.Forbidden)))

  /** Any signed-in caller, whatever their role (personal finance features). */
  val known: Auth[F] = req => user(req).map(_.map(_.id))
}

object Principal {

  def bearer[F[_]](req: Request[F]): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, token)) => token }
}
