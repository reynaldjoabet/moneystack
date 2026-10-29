package wealth.platform

import wealth.domain.*

import cats.data.ValidatedNel
import cats.effect.*
import cats.syntax.all.*
import io.circe.{Codec as JsonCodec, Decoder, Encoder}
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl

import java.time.{LocalDate, ZoneOffset}
import java.util.UUID

/**
 * Route plumbing shared by every module. `auth` resolves the caller to a user
 * id, or `None` to refuse; the host app decides what qualifies. `E` is the
 * module's domain error, mapped to a status by [[failed]].
 */
abstract class HttpSupport[F[_]: Async, E](auth: Request[F] => F[Option[UserId]]) extends Http4sDsl[F] {

  final class UuidVar[A](wrap: UUID => A) {
    def unapply(s: String): Option[A] = scala.util.Try(UUID.fromString(s)).toOption.map(wrap)
  }

  protected given QueryParamDecoder[LocalDate] =
    QueryParamDecoder[String].emap(s =>
      Either.catchNonFatal(LocalDate.parse(s)).leftMap(e => ParseFailure(s"invalid date: $s (expected YYYY-MM-DD)", e.getMessage))
    )

  protected def uuidParam[A](what: String, wrap: UUID => A): QueryParamDecoder[A] =
    QueryParamDecoder[String].emap(s =>
      Either.catchNonFatal(UUID.fromString(s)).bimap(_ => ParseFailure(s"invalid $what: $s", s), wrap)
    )

  protected def enumParam[A](parse: String => Either[String, A]): QueryParamDecoder[A] =
    QueryParamDecoder[String].emap(s => parse(s).leftMap(msg => ParseFailure(msg, s)))

  protected object AsOf  extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("asOf")
  protected object From  extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("from")
  protected object To    extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("to")
  protected object Limit extends OptionalQueryParamDecoderMatcher[Int]("limit")

  /** Runs `f` for an authorised caller, 403 otherwise. */
  protected def authed(req: Request[F])(f: UserId => F[Response[F]]): F[Response[F]] =
    auth(req).flatMap(_.fold(Forbidden())(f))

  protected def param[A](p: Option[ValidatedNel[ParseFailure, A]], default: => A): Either[String, A] =
    p.fold(Right(default))(_.toEither.leftMap(_.head.sanitized))

  protected def optParam[A](p: Option[ValidatedNel[ParseFailure, A]]): Either[String, Option[A]] =
    p.traverse(_.toEither.leftMap(_.head.sanitized))

  protected def limit(l: Option[Int]): Int = l.getOrElse(100).max(1).min(500)

  protected val today: F[LocalDate] = Async[F].delay(LocalDate.now(ZoneOffset.UTC))

  /** `from` defaults to the start of the year and `to` to today. */
  protected def period(from: Option[ValidatedNel[ParseFailure, LocalDate]], to: Option[ValidatedNel[ParseFailure, LocalDate]])(
      f: (LocalDate, LocalDate) => F[Response[F]]
  ): F[Response[F]] =
    today.flatMap(t => (param(from, t.withDayOfYear(1)), param(to, t)).tupled.fold(bad, f.tupled))

  protected def asOf(p: Option[ValidatedNel[ParseFailure, LocalDate]])(f: LocalDate => F[Response[F]]): F[Response[F]] =
    today.flatMap(t => param(p, t).fold(bad, f))

  protected def bad(msg: String): F[Response[F]] = BadRequest(Map("error" -> msg).asJson)

  /** Decodes the body, answering 400 with the failing field instead of letting the failure surface as a 500. */
  protected def decoded[A](req: Request[F])(f: A => F[Response[F]])(using EntityDecoder[F, A]): F[Response[F]] =
    req.attemptAs[A].value.flatMap {
      case Left(failure) => bad(failure.cause.map(_.getMessage).getOrElse(failure.message))
      case Right(a)      => f(a)
    }

  /** The status a domain error maps to. */
  protected def failed(err: E): F[Response[F]]

  protected def respond[A](res: Either[E, A])(ok: A => F[Response[F]]): F[Response[F]] =
    res.fold(failed, ok)

  /** `{"error": "..."}` for a domain error. */
  protected def errorBody(err: E): io.circe.Json = Map("error" -> err.toString).asJson

  def routes: HttpRoutes[F]
}

object JsonSupport {

  /** A JSON string codec over an enum's keys. */
  def enumCodec[A](render: A => String, parse: String => Either[String, A]): JsonCodec[A] =
    JsonCodec.from(Decoder.decodeString.emap(parse), Encoder.encodeString.contramap(render))
}
