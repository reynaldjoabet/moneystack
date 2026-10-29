package wealth.config

import wealth.domain.CurrencyCode

import cats.effect.Sync

final case class DbConfig(
    host: String, port: Int, user: String, password: Option[String], database: String, poolMax: Int
)

final case class HttpConfig(host: String, port: Int)

/** `baseCurrency` is the reporting currency; foreign-currency entries are valued in it. */
final case class LedgerConfig(baseCurrency: CurrencyCode)

final case class AppConfig(db: DbConfig, http: HttpConfig, ledger: LedgerConfig)

object AppConfig {
  def load[F[_]: Sync]: F[AppConfig] = Sync[F].delay {
    def env(name: String): Option[String]    = sys.env.get(name).filter(_.nonEmpty)
    def req(name: String): String            = env(name).getOrElse(sys.error(s"missing env var: $name"))
    def int(name: String, default: Int): Int = env(name).map(_.toInt).getOrElse(default)

    AppConfig(
      db = DbConfig(
        host     = env("DB_HOST").getOrElse("localhost"),
        port     = int("DB_PORT", 5432),
        user     = req("DB_USER"),
        password = env("DB_PASSWORD"),
        database = req("DB_NAME"),
        poolMax  = int("DB_POOL_MAX", 8)
      ),
      http = HttpConfig(
        host = env("HTTP_HOST").getOrElse("0.0.0.0"),
        port = int("HTTP_PORT", 8080)
      ),
      ledger = LedgerConfig(
        baseCurrency = CurrencyCode.either(env("LEDGER_BASE_CURRENCY").getOrElse("USD"))
                         .fold(err => sys.error(s"LEDGER_BASE_CURRENCY: $err"), identity)
      )
    )
  }
}
