package moneystack.platform

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import skunk.Session

import scala.util.control.NoStackTrace

/** Session access for work that spans several repositories in one transaction. */
final class Db[F[_]: MonadCancelThrow](val pool: Resource[F, Session[F]]) {

  def session[A](f: Session[F] => F[A]): F[A] = pool.use(f)

  def tx[A](f: Session[F] => F[A]): F[A] =
    pool.use(s => s.transaction.use(_ => f(s)))

  /** Runs `f` in a transaction that rolls back when it yields a domain error `E`. */
  def txE[E, A](f: Session[F] => EitherT[F, E, A]): F[Either[E, A]] =
    tx(s =>
      f(s).value.flatMap {
        case Left(err) => MonadCancelThrow[F].raiseError[A](Db.Rollback(err))
        case Right(a)  => MonadCancelThrow[F].pure(a)
      }
    ).map(_.asRight[E]).recover { case Db.Rollback(err) => Left(err.asInstanceOf[E]) }
}

object Db {
  private final case class Rollback(err: Any) extends RuntimeException(err.toString) with NoStackTrace
}
