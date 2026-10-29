package wealth.accounting.http

import wealth.domain.*
import wealth.accounting.service.BankingService

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

/** Bank rules and the bank feed. */
final class BankingRoutes[F[_]: Async](auth: Request[F] => F[Option[UserId]], banking: BankingService[F])
    extends RouteSupport[F](auth) {

  import AccountingJson.given

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- rules ----

    case req @ GET -> Root / "accountant" / "banking" / "rules" =>
      accountant(req)(_ => banking.rules.flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / "banking" / "rules" =>
      accountant(req)(_ => decoded[BankingService.NewRule](req)(b => banking.createRule(b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "accountant" / "banking" / "rules" / "run" =>
      accountant(req)(u => banking.runRules(u).flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / "banking" / "rules" / BankRuleIdVar(id) / "active" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ActiveBody](req)(b => banking.setRuleActive(id, b.active).flatMap(found => if (found) NoContent() else NotFound()))
      )

    // ---- feed ----

    case req @ POST -> Root / "accountant" / "banking" / "accounts" / AccountIdVar(account) / "transactions" =>
      accountant(req)(u =>
        decoded[List[BankingService.NewTransaction]](req)(rows =>
          banking.importTransactions(account, rows, u).flatMap(respond(_)(Ok(_)))
        )
      )

    case req @ GET -> Root / "accountant" / "banking" / "transactions" :? AccountParam(account) +& StatusParam(status) +& Limit(l) =>
      accountant(req)(_ =>
        (optParam(account), optParam(status)).tupled.fold(bad, { case (a, s) => banking.transactions(a, s, limit(l)).flatMap(Ok(_)) })
      )

    case req @ POST -> Root / "accountant" / "banking" / "transactions" / BankTxIdVar(id) / "categorize" =>
      accountant(req)(u =>
        decoded[AccountingJson.CategorizeBody](req)(b =>
          banking.categorize(id, b.accountId, b.contactId, u).flatMap(respond(_)(Ok(_)))
        )
      )

    case req @ POST -> Root / "accountant" / "banking" / "transactions" / BankTxIdVar(id) / "uncategorize" =>
      accountant(req)(u => banking.uncategorize(id, u).flatMap(respond(_)(Ok(_))))

    case req @ POST -> Root / "accountant" / "banking" / "transactions" / BankTxIdVar(id) / "exclude" =>
      accountant(req)(_ => banking.exclude(id).flatMap(respond(_)(Ok(_))))

    case req @ POST -> Root / "accountant" / "banking" / "transactions" / BankTxIdVar(id) / "restore" =>
      accountant(req)(_ => banking.restore(id).flatMap(respond(_)(Ok(_))))
  }
}
