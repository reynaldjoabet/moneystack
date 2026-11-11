package moneystack

import moneystack.auth.Tokens
import moneystack.config.*
import moneystack.db.*
import moneystack.external.*
import moneystack.http.*
import moneystack.service.*
import moneystack.accounting.AccountingModule
import moneystack.budget.BudgetModule
import moneystack.portfolio.PortfolioModule
import moneystack.domain.Role

import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.typelevel.log4cats.Logger
import skunk.Session

import scala.concurrent.duration.*

/** Every route of the app over one session pool. `Main` serves it; the integration suites call it directly. */
object App {

  /** `draining` turns readiness to 503 once shutdown begins. */
  def routes(
      cfg: AppConfig,
      pool: Resource[IO, Session[IO]],
      logger: Logger[IO],
      draining: IO[Boolean] = IO.pure(false)
  ): Resource[IO, HttpRoutes[IO]] =
    for {
      // ---- repositories ----
      usersR <- Resource.pure(Users.make[IO](pool))
      portR = Portfolios.make[IO](pool)
      holdR = Holdings.make[IO](pool)
      recsR = Recommendations.make[IO](pool)
      subsR = Subscriptions.make[IO](pool)

      // ---- external sandboxes ----
      market   <- Resource.eval(SandboxMarketData.make[IO](Map.empty))
      gateway  <- Resource.eval(PaymentGateway.sandbox[IO])
      crm      <- Resource.eval(Crm.sandbox[IO])
      telegram <- Resource.eval(Telegram.sandbox[IO](msg => logger.info(msg)))
      push     <- Resource.eval(Push.sandbox[IO](msg => logger.info(msg)))

      // ---- services ----
      notifications <- Supervisor[IO](await = true)
      books = AccountingModule.make[IO](pool, cfg.ledger.baseCurrency)
      _ <- Resource.eval(books.bootstrap)
      accounting        = LedgerAccounting.make[IO](usersR, books)
      portfolioBook     = PortfolioModule.make[IO](pool, cfg.ledger.baseCurrency)
      budgetBook        = BudgetModule.make[IO](pool, cfg.ledger.baseCurrency)
      portfolioSvc      = PortfolioService.make[IO](portR, holdR, market)
      recommendationSvc = RecommendationService.make[IO](recsR, portR, telegram, push, notifications, logger)
      subscriptionSvc   = SubscriptionService.make[IO](usersR, subsR, gateway, crm, accounting)

      // ---- auth ----
      tokens    = Tokens.make[IO](cfg.auth.jwtSecret.value, cfg.auth.issuer, cfg.auth.tokenTtl)
      principal = new Principal[IO](tokens, usersR)
    } yield new HealthRoutes[IO](pool, 2.seconds, draining).routes <+>
      new Routes[IO](
        principal,
        tokens,
        usersR,
        portR,
        holdR,
        recsR,
        subsR,
        market,
        portfolioSvc,
        recommendationSvc,
        subscriptionSvc
      ).routes <+>
      books.routes(principal.withRole(Role.Accountant)) <+>
      portfolioBook.routes(principal.known) <+>
      budgetBook.routes(principal.known)
}
