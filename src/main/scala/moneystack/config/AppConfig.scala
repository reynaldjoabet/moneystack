package moneystack.config

import moneystack.domain.CurrencyCode
import moneystack.platform.Migrations

import cats.effect.Async
import cats.syntax.all.*
import ciris.*

/** `migrateOnStart` applies pending Flyway migrations before the server starts. */
final case class DbConfig(
    host: String,
    port: Int,
    user: String,
    password: Option[Secret[String]],
    database: String,
    poolMax: Int,
    migrateOnStart: Boolean
) {
  def migrationTarget: Migrations.Target =
    Migrations.Target(s"jdbc:postgresql://$host:$port/$database", user, password.map(_.value))
}

final case class HttpConfig(host: String, port: Int)

/** `baseCurrency` is the reporting currency; foreign-currency entries are valued in it. */
final case class LedgerConfig(baseCurrency: CurrencyCode)

final case class AppConfig(db: DbConfig, http: HttpConfig, ledger: LedgerConfig)

object AppConfig {

  /** An environment variable, where an empty value counts as unset. */
  private def setting(name: String): ConfigValue[Effect, String] =
    env(name).flatMap(v => if (v.isEmpty) ConfigValue.missing(ConfigKey.env(name)) else ConfigValue.loaded(ConfigKey.env(name), v))

  private given ConfigDecoder[String, CurrencyCode] =
    ConfigDecoder[String].mapEither((key, s) =>
      CurrencyCode.either(s).leftMap(err => ConfigError(s"${key.fold("currency")(_.description)}: $err"))
    )

  private val db: ConfigValue[Effect, DbConfig] =
    (
      setting("DB_HOST").default("localhost"),
      setting("DB_PORT").as[Int].default(5432),
      setting("DB_USER"),
      setting("DB_PASSWORD").secret.option,
      setting("DB_NAME"),
      setting("DB_POOL_MAX").as[Int].default(8),
      setting("DB_MIGRATE_ON_START").as[Boolean].default(true)
    ).parMapN(DbConfig.apply)

  private val http: ConfigValue[Effect, HttpConfig] =
    (
      setting("HTTP_HOST").default("0.0.0.0"),
      setting("HTTP_PORT").as[Int].default(8080)
    ).parMapN(HttpConfig.apply)

  private val ledger: ConfigValue[Effect, LedgerConfig] =
    setting("LEDGER_BASE_CURRENCY").as[CurrencyCode].default(CurrencyCode.USD).map(LedgerConfig.apply)

  /** Reads the environment, reporting every missing or malformed variable at once. */
  def load[F[_]: Async]: F[AppConfig] = (db, http, ledger).parMapN(AppConfig.apply).load[F]
}
