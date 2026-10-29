package wealth.accounting.http

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.service.LedgerService

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

import java.time.Instant

/** Chart of accounts, journal, exchange rates, period lock, and the core statements. */
final class LedgerRoutes[F[_]: Async](auth: Request[F] => F[Option[UserId]], ledger: LedgerService[F])
    extends RouteSupport[F](auth) {

  import AccountingJson.given

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- chart of accounts ----

    case req @ GET -> Root / "accountant" / "ledger" / "accounts" =>
      accountant(req)(_ => ledger.accounts.flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / "ledger" / "accounts" =>
      accountant(req)(_ =>
        decoded[AccountingJson.CreateAccountBody](req) { b =>
          ledger.createAccount(LedgerService.NewAccount(b.code, b.name, b.accountType, b.currency, b.parentId))
            .flatMap(respond(_)(Created(_)))
        }
      )

    case req @ POST -> Root / "accountant" / "ledger" / "accounts" / AccountIdVar(id) / "active" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ActiveBody](req)(b => ledger.setActive(id, b.active).flatMap(_.fold(NotFound())(Ok(_))))
      )

    // ---- journal ----

    case req @ GET -> Root / "accountant" / "ledger" / "entries" :? From(from) +& To(to) +& Limit(l) =>
      accountant(req)(_ => period(from, to)((f, t) => ledger.entries(f, t, limit(l)).flatMap(Ok(_))))

    case req @ GET -> Root / "accountant" / "ledger" / "entries" / EntryIdVar(id) =>
      accountant(req)(_ => ledger.entry(id).flatMap(_.fold(NotFound())(Ok(_))))

    case req @ POST -> Root / "accountant" / "ledger" / "entries" =>
      accountant(req)(u =>
        decoded[AccountingJson.PostJournalBody](req) { b =>
          ledger.post(LedgerService.Draft(b.date, b.reference, b.memo, b.currency, b.exchangeRate, b.lines), u)
            .flatMap(respond(_)(Created(_)))
        }
      )

    case req @ POST -> Root / "accountant" / "ledger" / "entries" / EntryIdVar(id) / "reverse" =>
      accountant(req)(u =>
        decoded[AccountingJson.DateBody](req)(b => ledger.reverse(id, b.date, u).flatMap(respond(_)(Created(_))))
      )

    // ---- exchange rates ----

    case req @ PUT -> Root / "accountant" / "ledger" / "rates" =>
      accountant(req)(_ =>
        decoded[AccountingJson.SetRateBody](req)(b => ledger.setRate(FxRate(b.currency, b.date, b.rate)) *> NoContent())
      )

    case req @ GET -> Root / "accountant" / "ledger" / "rates" / currencySeg =>
      accountant(req)(_ =>
        CurrencyCode.either(currencySeg).fold(bad, c => ledger.rates(c).flatMap(Ok(_)))
      )

    // ---- period lock ----

    case req @ GET -> Root / "accountant" / "ledger" / "lock" =>
      accountant(req)(_ => ledger.lock.flatMap(_.fold(NoContent())(Ok(_))))

    case req @ PUT -> Root / "accountant" / "ledger" / "lock" =>
      accountant(req)(u =>
        decoded[AccountingJson.SetLockBody](req) { b =>
          (b.unlockFrom, b.unlockTo) match {
            case (Some(f), Some(t)) if f.isAfter(t) => bad("unlockFrom must not be after unlockTo")
            case (Some(_), None) | (None, Some(_))  => bad("unlockFrom and unlockTo must be given together")
            case _ =>
              for {
                now <- Async[F].delay(Instant.now())
                l    = LedgerLock(b.lockThrough, b.unlockFrom, b.unlockTo, b.reason, u, now)
                _   <- ledger.setLock(l)
                r   <- Ok(l)
              } yield r
          }
        }
      )

    case req @ DELETE -> Root / "accountant" / "ledger" / "lock" =>
      accountant(req)(_ => ledger.clearLock *> NoContent())

    // ---- statements ----

    case req @ GET -> Root / "accountant" / "reports" / "trial-balance" :? AsOf(d) =>
      accountant(req)(_ => asOf(d)(ledger.trialBalance(_).flatMap(Ok(_))))

    case req @ GET -> Root / "accountant" / "reports" / "balance-sheet" :? AsOf(d) =>
      accountant(req)(_ => asOf(d)(ledger.balanceSheet(_).flatMap(Ok(_))))

    case req @ GET -> Root / "accountant" / "reports" / "profit-and-loss" :? From(from) +& To(to) =>
      accountant(req)(_ => period(from, to)((f, t) => ledger.profitAndLoss(f, t).flatMap(respond(_)(Ok(_)))))

    case req @ GET -> Root / "accountant" / "reports" / "general-ledger" :? From(from) +& To(to) +& AccountParam(account) =>
      accountant(req)(_ =>
        today.flatMap { t =>
          (param(from, t.withDayOfMonth(1)), param(to, t), optParam(account)).tupled match {
            case Left(err)        => bad(err)
            case Right((f, u, a)) => ledger.generalLedger(f, u, a).flatMap(respond(_)(Ok(_)))
          }
        }
      )
  }
}
