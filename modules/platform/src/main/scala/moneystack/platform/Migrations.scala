package moneystack.platform

import cats.effect.Sync
import cats.syntax.all.*
import org.flywaydb.core.Flyway

/**
 * Flyway migrations, one history table per module. A module's scripts live in
 * `db/migration/<module>/V<n>__<title>.sql` on the classpath and are never
 * edited once deployed: a mistake is fixed by a new version.
 */
object Migrations {

  final case class Target(jdbcUrl: String, user: String, password: Option[String])

  /** Applies each module's pending migrations, in order; returns how many ran. */
  def run[F[_]: Sync](target: Target, modules: List[String]): F[Int] =
    modules.traverse(m => Sync[F].blocking(flyway(target, m).migrate().migrationsExecuted)).map(_.sum)

  private def flyway(target: Target, module: String): Flyway =
    Flyway.configure()
      .dataSource(target.jdbcUrl, target.user, target.password.orNull)
      .locations(s"classpath:db/migration/$module")
      .table(s"flyway_history_$module")
      // Postgres may still be starting (a fresh container, or the app deployed alongside its database):
      // retry the connection with backoff instead of failing the first attempt.
      .connectRetries(10)
      // Modules share the public schema, so it already holds other modules' tables on a module's first run.
      // Baselining at 0 starts the history there without skipping V1.
      .baselineOnMigrate(true)
      .baselineVersion("0")
      .load()
}
