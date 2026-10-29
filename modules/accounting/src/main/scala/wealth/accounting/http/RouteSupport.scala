package wealth.accounting.http

import wealth.domain.*
import wealth.accounting.domain.*

import cats.data.ValidatedNel
import cats.effect.*
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl

import java.time.{LocalDate, ZoneOffset}
import java.util.UUID

/**
 * Shared plumbing for the accounting routes. `auth` resolves the caller to an
 * accountant's user id, or `None` to refuse; the host app decides who that is.
 */
abstract class RouteSupport[F[_]: Async](auth: Request[F] => F[Option[UserId]]) extends Http4sDsl[F] {

  final class UuidVar[A](wrap: UUID => A) {
    def unapply(s: String): Option[A] = scala.util.Try(UUID.fromString(s)).toOption.map(wrap)
  }

  protected val AccountIdVar  = new UuidVar(AccountId.assume)
  protected val EntryIdVar    = new UuidVar(JournalEntryId.assume)
  protected val ContactIdVar  = new UuidVar(ContactId.assume)
  protected val TaxRateIdVar  = new UuidVar(TaxRateId.assume)
  protected val ItemIdVar     = new UuidVar(ItemId.assume)
  protected val DocumentIdVar = new UuidVar(DocumentId.assume)
  protected val SettlementIdVar = new UuidVar(SettlementId.assume)
  protected val BankTxIdVar   = new UuidVar(BankTransactionId.assume)
  protected val BankRuleIdVar = new UuidVar(BankRuleId.assume)

  protected given QueryParamDecoder[LocalDate] =
    QueryParamDecoder[String].emap(s =>
      Either.catchNonFatal(LocalDate.parse(s)).leftMap(e => ParseFailure(s"invalid date: $s (expected YYYY-MM-DD)", e.getMessage))
    )

  private def uuidParam[A](what: String, wrap: UUID => A): QueryParamDecoder[A] =
    QueryParamDecoder[String].emap(s =>
      Either.catchNonFatal(UUID.fromString(s)).bimap(_ => ParseFailure(s"invalid $what: $s", s), wrap)
    )

  private def enumParam[A](parse: String => Either[String, A]): QueryParamDecoder[A] =
    QueryParamDecoder[String].emap(s => parse(s).leftMap(msg => ParseFailure(msg, s)))

  protected given QueryParamDecoder[AccountId]    = uuidParam("account id", AccountId.assume)
  protected given QueryParamDecoder[ContactId]    = uuidParam("contact id", ContactId.assume)
  protected given QueryParamDecoder[ContactKind]  = enumParam(ContactKind.parse)
  protected given QueryParamDecoder[BankTxStatus] = enumParam(BankTxStatus.parse)

  protected object AsOf         extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("asOf")
  protected object From         extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("from")
  protected object To           extends OptionalValidatingQueryParamDecoderMatcher[LocalDate]("to")
  protected object AccountParam extends OptionalValidatingQueryParamDecoderMatcher[AccountId]("account")
  protected object ContactParam extends OptionalValidatingQueryParamDecoderMatcher[ContactId]("contact")
  protected object KindParam    extends OptionalValidatingQueryParamDecoderMatcher[ContactKind]("kind")
  protected object StatusParam  extends OptionalValidatingQueryParamDecoderMatcher[BankTxStatus]("status")
  protected object Limit        extends OptionalQueryParamDecoderMatcher[Int]("limit")

  protected def accountant(req: Request[F])(f: UserId => F[Response[F]]): F[Response[F]] =
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

  protected def failed(err: LedgerError): F[Response[F]] = {
    import LedgerError.*
    val body = Map("error" -> err.toString).asJson
    err match {
      case EntryNotFound(_) | DocumentNotFound(_) | SettlementNotFound(_) | BankTransactionNotFound(_) => NotFound(body)
      case PeriodLocked(_, _) | AlreadyReversed(_, _) | DuplicateAccountCode(_) | DuplicateCode(_, _) |
           DuplicatePosting(_, _) | AlreadyVoided(_) | DocumentHasPayments | InvalidTransactionState(_, _) |
           InsufficientStock(_, _, _) | StockAlreadyConsumed(_, _, _) | OverAllocation(_, _, _)  => Conflict(body)
      case _                                                                                     => BadRequest(body)
    }
  }

  protected def respond[A](res: Either[LedgerError, A])(ok: A => F[Response[F]]): F[Response[F]] =
    res.fold(failed, ok)

  def routes: HttpRoutes[F]
}
