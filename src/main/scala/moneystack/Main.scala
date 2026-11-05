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
import fs2.io.net.tls.TLSParameters
import skunk.{SSL, Session}

import javax.net.ssl.SNIHostName

object Main extends IOApp.Simple {

  /**
   * `System` also checks the certificate names `DB_HOST`, as pgjdbc's verify-full does for Flyway: skunk's default
   * only checks that a trusted CA issued it, which any other certificate from that CA would pass.
   */
  private[moneystack] def sslFor(mode: DbSsl, host: String): SSL = mode match {
    case DbSsl.Disabled => SSL.None
    case DbSsl.Trusted  => SSL.Trusted
    case DbSsl.System   =>
      // The TLS engine isn't told the peer's name, so the check takes it from the SNI name.
      SSL.System.withTLSParameters(
        TLSParameters(endpointIdentificationAlgorithm = Some("HTTPS"), serverNames = Some(List(new SNIHostName(host))))
      )
  }

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
      draining <- Resource.eval(Ref[IO].of(false))
      routes   <- App.routes(cfg, pool, logger, draining.get)
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
      // Released before the server (resources release in reverse): report not-ready, then give load balancers
      // `drainDelay` to notice before Ember stops accepting connections and drains in-flight requests.
      _ <- Resource.onFinalize(
        draining.set(true) *> logger.info(s"Shutting down: draining for ${cfg.http.drainDelay}") *> IO.sleep(cfg.http.drainDelay)
      )
    } yield ()

  private def sessionPool(db: DbConfig, telemetry: Telemetry): Resource[IO, Resource[IO, Session[IO]]] = {
    given TracerProvider[IO] = telemetry.tracerProvider
    given MeterProvider[IO]  = telemetry.meterProvider
    val base                 = Session.Builder[IO]
      .withHost(db.host)
      .withPort(db.port)
      .withDatabase(db.database)
      .withSSL(sslFor(db.ssl, db.host))
      .withConnectionParameters(
        Session.DefaultConnectionParameters ++ Map(
          "statement_timeout"                   -> db.statementTimeout.toMillis.toString,
          "idle_in_transaction_session_timeout" -> db.idleInTransactionTimeout.toMillis.toString
        )
      )
    db.password
      .fold(base.withUser(db.user))(p => base.withUserAndPassword(db.user, p.value))
      .pooled(db.poolMax)
  }
}
