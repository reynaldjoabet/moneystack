package moneystack

import moneystack.config.*
import moneystack.db.*
import moneystack.external.*
import moneystack.http.*
import moneystack.service.*
import moneystack.accounting.AccountingModule
import moneystack.budget.BudgetModule
import moneystack.portfolio.PortfolioModule
import moneystack.domain.Role
import moneystack.platform.Migrations

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import skunk.Session

object Main extends IOApp.Simple {

  given TracerProvider[IO] = TracerProvider.noop[IO]
  given MeterProvider[IO]  = MeterProvider.noop[IO]

  /** The app's own migrations, then each module's: the order they are applied in. */
  val migrations: List[String] =
    List("app", AccountingModule.migrations, PortfolioModule.migrations, BudgetModule.migrations)

  def run: IO[Unit] = {
    val logger = Slf4jLogger.getLogger[IO]

    val server: Resource[IO, Unit] =
      for {
        cfg <- Resource.eval(AppConfig.load[IO])

        _ <- Resource.eval(
          if (cfg.db.migrateOnStart)
            Migrations.run[IO](cfg.db.migrationTarget, migrations).flatMap(n => logger.info(s"Applied $n migration(s)"))
          else logger.info("Skipping migrations: DB_MIGRATE_ON_START=false")
        )

        pool <- {
          val base = Session.Builder[IO]
            .withHost(cfg.db.host)
            .withPort(cfg.db.port)
            .withDatabase(cfg.db.database)
          cfg.db.password
            .fold(base.withUser(cfg.db.user))(p => base.withUserAndPassword(cfg.db.user, p.value))
            .pooled(cfg.db.poolMax)
        }

        // ---- repositories ----
        usersR = Users.make[IO](pool)
        portR  = Portfolios.make[IO](pool)
        holdR  = Holdings.make[IO](pool)
        recsR  = Recommendations.make[IO](pool)
        subsR  = Subscriptions.make[IO](pool)

        // ---- external sandboxes ----
        market   <- Resource.eval(SandboxMarketData.make[IO](Map.empty))
        gateway  <- Resource.eval(PaymentGateway.sandbox[IO])
        crm      <- Resource.eval(Crm.sandbox[IO])
        tax      <- Resource.eval(Tax.sandbox[IO])
        telegram <- Resource.eval(Telegram.sandbox[IO](msg => logger.info(msg)))
        push     <- Resource.eval(Push.sandbox[IO](msg => logger.info(msg)))
        errors   <- Resource.eval(ErrorMonitoring.sandbox[IO])
        support  <- Resource.eval(SupportSink.sandbox[IO])

        // ---- services ----
        books = AccountingModule.make[IO](pool, cfg.ledger.baseCurrency)
        _ <- Resource.eval(books.bootstrap)
        accounting        = LedgerAccounting.make[IO](usersR, books)
        portfolioBook     = PortfolioModule.make[IO](pool, cfg.ledger.baseCurrency)
        budgetBook        = BudgetModule.make[IO](pool, cfg.ledger.baseCurrency)
        portfolioSvc      = PortfolioService.make[IO](portR, holdR, market)
        recommendationSvc = RecommendationService.make[IO](recsR, portR, usersR, telegram, push)
        subscriptionSvc   = SubscriptionService.make[IO](usersR, subsR, gateway, crm, accounting)

        // ---- HTTP ----
        routes =
          new HealthRoutes[IO](pool).routes <+>
            new Routes[IO](
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
            books.routes(Principal.withRole[IO](usersR, Role.Accountant)) <+>
            portfolioBook.routes(Principal.known[IO](usersR)) <+>
            budgetBook.routes(Principal.known[IO](usersR))
        app = Logger.httpApp(logHeaders = true, logBody = false)(routes.orNotFound)

        host <- Resource.eval(IO.fromOption(Host.fromString(cfg.http.host))(new IllegalArgumentException("bad host")))
        port <- Resource.eval(IO.fromOption(Port.fromInt(cfg.http.port))(new IllegalArgumentException("bad port")))

        _ <- EmberServerBuilder.default[IO].withHost(host).withPort(port).withHttpApp(app).build
        _ <- Resource.eval(logger.info(s"Listening on http://$host:$port"))
        // Keep refs to silence unused-warning on tax/errors/support — they're real seams used by other workers.
        _ <- Resource.eval(IO.pure((tax, errors, support)))
      } yield ()

    server.useForever
  }
}
