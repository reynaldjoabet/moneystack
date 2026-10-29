package wealth.accounting.http

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.platform.HttpSupport

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

/**
 * Shared plumbing for the accounting routes. `auth` resolves the caller to an
 * accountant's user id, or `None` to refuse; the host app decides who that is.
 */
abstract class RouteSupport[F[_]: Async](auth: Request[F] => F[Option[UserId]]) extends HttpSupport[F, LedgerError](auth) {

  protected val AccountIdVar  = new UuidVar(AccountId.assume)
  protected val EntryIdVar    = new UuidVar(JournalEntryId.assume)
  protected val ContactIdVar  = new UuidVar(ContactId.assume)
  protected val TaxRateIdVar  = new UuidVar(TaxRateId.assume)
  protected val ItemIdVar     = new UuidVar(ItemId.assume)
  protected val DocumentIdVar = new UuidVar(DocumentId.assume)
  protected val SettlementIdVar = new UuidVar(SettlementId.assume)
  protected val BankTxIdVar   = new UuidVar(BankTransactionId.assume)
  protected val BankRuleIdVar = new UuidVar(BankRuleId.assume)

  protected given QueryParamDecoder[AccountId]    = uuidParam("account id", AccountId.assume)
  protected given QueryParamDecoder[ContactId]    = uuidParam("contact id", ContactId.assume)
  protected given QueryParamDecoder[ContactKind]  = enumParam(ContactKind.parse)
  protected given QueryParamDecoder[BankTxStatus] = enumParam(BankTxStatus.parse)

  protected object AccountParam extends OptionalValidatingQueryParamDecoderMatcher[AccountId]("account")
  protected object ContactParam extends OptionalValidatingQueryParamDecoderMatcher[ContactId]("contact")
  protected object KindParam    extends OptionalValidatingQueryParamDecoderMatcher[ContactKind]("kind")
  protected object StatusParam  extends OptionalValidatingQueryParamDecoderMatcher[BankTxStatus]("status")

  protected def accountant(req: Request[F])(f: UserId => F[Response[F]]): F[Response[F]] = authed(req)(f)

  protected def failed(err: LedgerError): F[Response[F]] = {
    import LedgerError.*
    val body = errorBody(err)
    err match {
      case EntryNotFound(_) | DocumentNotFound(_) | SettlementNotFound(_) | BankTransactionNotFound(_) => NotFound(body)
      case PeriodLocked(_, _) | AlreadyReversed(_, _) | DuplicateAccountCode(_) | DuplicateCode(_, _) |
           DuplicatePosting(_, _) | AlreadyVoided(_) | DocumentHasPayments | InvalidTransactionState(_, _) |
           InsufficientStock(_, _, _) | StockAlreadyConsumed(_, _, _) | OverAllocation(_, _, _)  => Conflict(body)
      case _                                                                                     => BadRequest(body)
    }
  }
}
