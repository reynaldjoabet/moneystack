package moneystack.it

import moneystack.Main
import moneystack.http.HealthRoutes

import cats.effect.*
import org.http4s.*
import org.http4s.implicits.*
import skunk.Session

import scala.concurrent.duration.*

/** One database migrated the way the app migrates at startup: the app's schema, then every module's. */
class AppIntegrationSuite extends moneystack.testkit.PostgresSuite[HealthRoutes[IO]] {

  def migrations: List[String] = Main.migrations

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, HealthRoutes[IO]] =
    Resource.pure(new HealthRoutes[IO](pool, 2.seconds, IO.pure(false)))

  test("each module records V1 in its own history table") {
    Main.migrations.foldLeft(IO.unit) { (acc, module) =>
      acc >> assertIO(scalar(s"SELECT count(*) FROM flyway_history_$module WHERE version = '1' AND success"), 1L, module)
    }
  }

  test("foreign keys into large tables are indexed, and every index is valid") {
    for {
      // V2 (V6 for the app) builds them CONCURRENTLY, which fails outright if Flyway runs it in a transaction.
      fkIndexes <- scalar("SELECT count(*) FROM pg_indexes WHERE indexname LIKE '%\\_fk\\_idx'")
      invalid   <- scalar("SELECT count(*) FROM pg_index WHERE NOT indisvalid")
      applied   <- scalar(
        """SELECT (SELECT count(*) FROM flyway_history_budget WHERE version = '2' AND success)
                 + (SELECT count(*) FROM flyway_history_portfolio WHERE version = '2' AND success)
                 + (SELECT count(*) FROM flyway_history_accounting WHERE version = '2' AND success)
                 + (SELECT count(*) FROM flyway_history_app WHERE version = '6' AND success)"""
      )
    } yield {
      assertEquals(fkIndexes, 26L)
      assertEquals(invalid, 0L)
      assertEquals(applied, 4L)
    }
  }

  test("tables from every module exist side by side") {
    assertIO(
      scalar(
        """SELECT count(*) FROM information_schema.tables
           WHERE (table_schema, table_name) IN (('public', 'users'), ('public', 'journal_entries'),
                                                ('portfolio', 'accounts'), ('budget', 'wallets'))"""
      ),
      4L
    )
  }

  test("readiness is 200 when Postgres answers") {
    assertIO(env.routes.orNotFound.run(Request[IO](Method.GET, uri"/health/ready")).map(_.status), Status.Ok)
  }
}
