package moneystack.http

import moneystack.domain.*
import moneystack.db.Users

import cats.effect.*
import cats.syntax.all.*
import org.http4s.Request

import java.util.UUID

/** Resolves the `X-User-Id` principal (set upstream by the JWT middleware), as in [[Routes]]. */
object Principal {

  def userId[F[_]](req: Request[F]): Option[UserId] =
    req.headers.get(org.typelevel.ci.CIString("X-User-Id"))
      .flatMap(h => scala.util.Try(UUID.fromString(h.head.value)).toOption.map(UserId.assume))

  /** The caller's id when they hold `role`. */
  def withRole[F[_]: Sync](users: Users[F], role: Role)(req: Request[F]): F[Option[UserId]] =
    userId(req).flatTraverse(uid => users.find(uid).map(_.filter(_.role == role).map(_.id)))

  /** The caller's id when they are a known user, whatever their role (personal finance features). */
  def known[F[_]: Sync](users: Users[F])(req: Request[F]): F[Option[UserId]] =
    userId(req).flatTraverse(uid => users.find(uid).map(_.map(_.id)))
}
