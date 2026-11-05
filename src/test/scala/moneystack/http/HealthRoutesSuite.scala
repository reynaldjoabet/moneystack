package moneystack.http

import cats.effect.*
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.implicits.*
import skunk.Session

import scala.concurrent.duration.*

class HealthRoutesSuite extends CatsEffectSuite {

  private def status(pool: Resource[IO, Session[IO]], path: Uri): IO[Status] =
    new HealthRoutes[IO](pool, 200.millis, IO.pure(false)).routes.orNotFound.run(Request[IO](Method.GET, path)).map(_.status)

  private val refused: Resource[IO, Session[IO]] = Resource.eval(IO.raiseError(new java.net.ConnectException("refused")))
  private val hanging: Resource[IO, Session[IO]] = Resource.eval(IO.never)

  test("liveness does not touch the database") {
    assertIO(status(refused, uri"/health/live"), Status.Ok) >>
      assertIO(status(refused, uri"/health"), Status.Ok)
  }

  test("readiness is 503 when Postgres refuses connections") {
    assertIO(status(refused, uri"/health/ready"), Status.ServiceUnavailable)
  }

  test("readiness is 503 when Postgres does not answer in time") {
    assertIO(status(hanging, uri"/health/ready"), Status.ServiceUnavailable)
  }

  test("readiness is 503 once draining, without asking Postgres; liveness stays 200") {
    val routes = new HealthRoutes[IO](hanging, 200.millis, IO.pure(true)).routes.orNotFound
    assertIO(routes.run(Request[IO](Method.GET, uri"/health/ready")).map(_.status), Status.ServiceUnavailable) >>
      assertIO(routes.run(Request[IO](Method.GET, uri"/health/live")).map(_.status), Status.Ok)
  }
}
