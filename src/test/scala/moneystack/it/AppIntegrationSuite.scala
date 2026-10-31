package moneystack.it

import moneystack.Main
import moneystack.http.HealthRoutes

import cats.effect.*
import org.http4s.*
import org.http4s.implicits.*
import skunk.Session

/** One database migrated the way the app migrates at startup: the app's schema, then every module's. */
class AppIntegrationSuite extends moneystack.testkit.PostgresSuite[HealthRoutes[IO]] {

  def migrations: List[String] = Main.migrations

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, HealthRoutes[IO]] =
    Resource.pure(new HealthRoutes[IO](pool))

  test("each module records V1 in its own history table") {
    Main.migrations.foldLeft(IO.unit) { (acc, module) =>
      acc >> assertIO(scalar(s"SELECT count(*) FROM flyway_history_$module WHERE version = '1' AND success"), 1L, module)
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
