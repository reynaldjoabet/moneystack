package moneystack

import moneystack.auth.Passwords
import moneystack.config.*
import moneystack.http.Middleware
import moneystack.accounting.AccountingModule
import moneystack.budget.BudgetModule
import moneystack.portfolio.PortfolioModule
import moneystack.platform.Migrations

import cats.effect.*
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import skunk.{SSL, Session}

object Main extends IOApp.Simple {

  /** The app's own migrations, then each module's: the order they are applied in. */
  val migrations: List[String] =
    List("app", AccountingModule.migrations, PortfolioModule.migrations, BudgetModule.migrations)

  def run: IO[Unit] = {
    val logger = Slf4jLogger.getLogger[IO]
    val server = for {
      cfg       <- Resource.eval(AppConfig.load[IO])
      telemetry <- Telemetry.make(cfg.telemetry, runtime)
      _         <- serve(cfg, telemetry, logger)
    } yield ()
    server.useForever
  }

  private def serve(cfg: AppConfig, telemetry: Telemetry, logger: Logger[IO]): Resource[IO, Unit] =
    for {
      _ <- Resource.eval(
        if (cfg.db.migrateOnStart)
          Migrations.run[IO](cfg.db.migrationTarget, migrations).flatMap(n => logger.info(s"Applied $n migration(s)"))
        else logger.info("Skipping migrations: DB_MIGRATE_ON_START=false")
      )
      pool     <- sessionPool(cfg.db, telemetry)
      _        <- Resource.eval(Passwords.warmUp[IO])
      routes   <- App.routes(cfg, pool, logger)
      measured <- Resource.eval(Telemetry.metrics(telemetry)(routes))
      app      <- Resource.eval(Telemetry.tracing(telemetry)(Middleware[IO](cfg.http, logger)(measured.orNotFound)))
      _        <- EmberServerBuilder.default[IO]
        .withHost(cfg.http.host)
        .withPort(cfg.http.port)
        .withIdleTimeout(cfg.http.idleTimeout)
        .withShutdownTimeout(cfg.http.shutdownTimeout)
        .withHttpApp(app)
        .build
      _ <- Resource.eval(logger.info(s"Listening on http://${cfg.http.host}:${cfg.http.port}"))
    } yield ()

  private def sessionPool(db: DbConfig, telemetry: Telemetry): Resource[IO, Resource[IO, Session[IO]]] = {
    given TracerProvider[IO] = telemetry.tracerProvider
    given MeterProvider[IO]  = telemetry.meterProvider
    val ssl                  = db.ssl match {
      case DbSsl.Disabled => SSL.None
      case DbSsl.Trusted  => SSL.Trusted
      case DbSsl.System   => SSL.System
    }
    val base = Session.Builder[IO]
      .withHost(db.host)
      .withPort(db.port)
      .withDatabase(db.database)
      .withSSL(ssl)
    db.password
      .fold(base.withUser(db.user))(p => base.withUserAndPassword(db.user, p.value))
      .pooled(db.poolMax)
  }
}
