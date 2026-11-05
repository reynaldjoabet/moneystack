package moneystack.config

import moneystack.auth.Tokens
import moneystack.domain.CurrencyCode
import moneystack.platform.Migrations

import cats.effect.Async
import cats.syntax.all.*
import ciris.*
import com.comcast.ip4s.*

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.FiniteDuration

/** How the app reaches Postgres over TLS: not at all, encrypted without checking the certificate, or verified by the JVM. */
enum DbSsl {
  case Disabled, Trusted, System
}

/**
 * `migrateOnStart` applies pending Flyway migrations before the server starts. The timeouts are set on every
 * app session: Postgres cancels a statement running past `statementTimeout`, and ends a session left idle inside a
 * transaction past `idleInTransactionTimeout`, so neither can hold locks or a pool slot indefinitely.
 */
final case class DbConfig(
    host: String,
    port: Int,
    user: String,
    password: Option[Secret[String]],
    database: String,
    poolMax: Int,
    migrateOnStart: Boolean,
    ssl: DbSsl,
    statementTimeout: FiniteDuration,
    idleInTransactionTimeout: FiniteDuration
) {

  /** pgjdbc's equivalent of `ssl`: `DefaultJavaSSLFactory` verifies against the JVM's trust store, like skunk's `SSL.System`. */
  private def sslParams: String = ssl match {
    case DbSsl.Disabled => ""
    case DbSsl.Trusted  => "?sslmode=require&sslfactory=org.postgresql.ssl.NonValidatingFactory"
    case DbSsl.System   => "?sslmode=verify-full&sslfactory=org.postgresql.ssl.DefaultJavaSSLFactory"
  }

  def migrationTarget: Migrations.Target =
    Migrations.Target(s"jdbc:postgresql://$host:$port/$database$sslParams", user, password.map(_.value))
}

/**
 * An empty `corsOrigins` serves no CORS headers, so browsers only call the API from its own origin. On shutdown the
 * server reports not-ready for `drainDelay` before it stops accepting connections, so load balancers stop sending
 * new requests first.
 */
final case class HttpConfig(
    host: Host,
    port: Port,
    requestTimeout: FiniteDuration,
    idleTimeout: FiniteDuration,
    shutdownTimeout: FiniteDuration,
    maxBodyBytes: Long,
    corsOrigins: List[String],
    drainDelay: FiniteDuration
)

final case class AuthConfig(jwtSecret: Secret[String], issuer: String, tokenTtl: FiniteDuration)

/** When enabled, traces and metrics go out over OTLP, configured by the standard `OTEL_*` variables. */
final case class TelemetryConfig(enabled: Boolean)

/** `baseCurrency` is the reporting currency; foreign-currency entries are valued in it. */
final case class LedgerConfig(baseCurrency: CurrencyCode)

final case class AppConfig(db: DbConfig, http: HttpConfig, auth: AuthConfig, telemetry: TelemetryConfig, ledger: LedgerConfig)

object AppConfig {

  /** An environment variable, where an empty value counts as unset. */
  private def setting(name: String): ConfigValue[Effect, String] =
    env(name).flatMap(v => if (v.isEmpty) ConfigValue.missing(ConfigKey.env(name)) else ConfigValue.loaded(ConfigKey.env(name), v))

  private def describe(key: Option[ConfigKey], what: String): String = key.fold(what)(_.description)

  private given ConfigDecoder[String, CurrencyCode] =
    ConfigDecoder[String].mapEither((key, s) => CurrencyCode.either(s).leftMap(err => ConfigError(s"${describe(key, "currency")}: $err")))

  private given ConfigDecoder[String, Host] =
    ConfigDecoder[String].mapOption("host")(Host.fromString)

  private given ConfigDecoder[String, Port] =
    ConfigDecoder[String, Int].mapOption("port")(Port.fromInt)

  private given ConfigDecoder[String, DbSsl] =
    ConfigDecoder[String].mapEither((key, s) =>
      s.toLowerCase match {
        case "disable" => Right(DbSsl.Disabled)
        case "trusted" => Right(DbSsl.Trusted)
        case "system"  => Right(DbSsl.System)
        case other     => Left(ConfigError(s"${describe(key, "ssl")}: expected disable, trusted or system, got $other"))
      }
    )

  /** A comma-separated list; blank entries are dropped. */
  private def list(name: String): ConfigValue[Effect, List[String]] =
    setting(name).map(_.split(',').map(_.trim).filter(_.nonEmpty).toList).default(Nil)

  /** At least [[Tokens.MinSecretBytes]], checked before it becomes a `Secret` so the error never shows the value. */
  private val jwtSecret: ConfigValue[Effect, Secret[String]] =
    setting("AUTH_JWT_SECRET").flatMap { s =>
      if (s.getBytes(UTF_8).length >= Tokens.MinSecretBytes) ConfigValue.loaded(ConfigKey.env("AUTH_JWT_SECRET"), s)
      else ConfigValue.failed(ConfigError(s"AUTH_JWT_SECRET must be at least ${Tokens.MinSecretBytes} bytes"))
    }.secret

  private val db: ConfigValue[Effect, DbConfig] =
    (
      setting("DB_HOST").default("localhost"),
      setting("DB_PORT").as[Int].default(5432),
      setting("DB_USER"),
      setting("DB_PASSWORD").secret.option,
      setting("DB_NAME"),
      setting("DB_POOL_MAX").as[Int].default(8),
      setting("DB_MIGRATE_ON_START").as[Boolean].default(true),
      setting("DB_SSL").as[DbSsl].default(DbSsl.Disabled),
      setting("DB_STATEMENT_TIMEOUT").as[FiniteDuration].default(FiniteDuration(30, "s")),
      setting("DB_IDLE_IN_TRANSACTION_TIMEOUT").as[FiniteDuration].default(FiniteDuration(60, "s"))
    ).parMapN(DbConfig.apply)

  private val http: ConfigValue[Effect, HttpConfig] =
    (
      setting("HTTP_HOST").as[Host].default(host"0.0.0.0"),
      setting("HTTP_PORT").as[Port].default(port"8080"),
      setting("HTTP_REQUEST_TIMEOUT").as[FiniteDuration].default(FiniteDuration(30, "s")),
      setting("HTTP_IDLE_TIMEOUT").as[FiniteDuration].default(FiniteDuration(60, "s")),
      setting("HTTP_SHUTDOWN_TIMEOUT").as[FiniteDuration].default(FiniteDuration(30, "s")),
      setting("HTTP_MAX_BODY_BYTES").as[Long].default(1024L * 1024),
      list("HTTP_CORS_ORIGINS"),
      setting("HTTP_DRAIN_DELAY").as[FiniteDuration].default(FiniteDuration(5, "s"))
    ).parMapN(HttpConfig.apply)

  private val auth: ConfigValue[Effect, AuthConfig] =
    (
      jwtSecret,
      setting("AUTH_ISSUER").default("moneystack"),
      setting("AUTH_TOKEN_TTL").as[FiniteDuration].default(FiniteDuration(1, "h"))
    ).parMapN(AuthConfig.apply)

  private val telemetry: ConfigValue[Effect, TelemetryConfig] =
    setting("TELEMETRY_ENABLED").as[Boolean].default(false).map(TelemetryConfig.apply)

  private val ledger: ConfigValue[Effect, LedgerConfig] =
    setting("LEDGER_BASE_CURRENCY").as[CurrencyCode].default(CurrencyCode.USD).map(LedgerConfig.apply)

  /** Reads the environment, reporting every missing or malformed variable at once. */
  def load[F[_]: Async]: F[AppConfig] = (db, http, auth, telemetry, ledger).parMapN(AppConfig.apply).load[F]
}
