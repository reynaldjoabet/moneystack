package moneystack.testkit

import cats.effect.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.testcontainers.postgresql.PostgreSQLContainer
import org.testcontainers.utility.{DockerImageName, MountableFile}
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import skunk.Session
import skunk.implicits.*

import scala.concurrent.duration.*

/** Throwaway Postgres containers with a module's schema applied, as a deployment would apply it. */
object Postgres {

  private given TracerProvider[IO] = TracerProvider.noop[IO]
  private given MeterProvider[IO]  = MeterProvider.noop[IO]

  private val container: Resource[IO, PostgreSQLContainer] =
    Resource.make(IO.blocking {
      val c = new PostgreSQLContainer(DockerImageName.parse("postgres:18-alpine"))
      c.start()
      c
    })(c => IO.blocking(c.stop()))

  /** Runs one classpath SQL resource through psql inside the container. */
  private def applyScript(c: PostgreSQLContainer, resource: String): IO[Unit] = IO.blocking {
    val target = "/tmp/" + resource.replace('/', '_')
    c.copyFileToContainer(MountableFile.forClasspathResource(resource), target)
    val r = c.execInContainer("psql", "-v", "ON_ERROR_STOP=1", "-q", "-U", c.getUsername, "-d", c.getDatabaseName, "-f", target)
    if (r.getExitCode != 0) throw new IllegalStateException(s"$resource failed: ${r.getStderr}")
  }

  /**
   * A session pool over a fresh database with `schemas` applied in order —
   * twice, since every schema script must be idempotent.
   */
  def pool(schemas: List[String]): Resource[IO, Resource[IO, Session[IO]]] =
    for {
      c    <- container
      _    <- Resource.eval((schemas ++ schemas).traverse_(applyScript(c, _)))
      pool <- Session.Builder[IO]
                .withHost(c.getHost)
                .withPort(c.getMappedPort(5432).intValue)
                .withUserAndPassword(c.getUsername, c.getPassword)
                .withDatabase(c.getDatabaseName)
                .pooled(8)
    } yield pool
}

/** A suite with one database for all its tests; `Env` is whatever the module builds over the pool. */
abstract class PostgresSuite[Env] extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 3.minutes

  /** Classpath schema scripts, applied in order. */
  def schemas: List[String]

  /** Builds the module under test over the pool. */
  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Env]

  val db = ResourceSuiteLocalFixture("postgres", Postgres.pool(schemas).flatMap(p => build(p).map(p -> _)))

  override def munitFixtures = List(db)

  def env: Env                          = db()._2
  def sessions: Resource[IO, Session[IO]] = db()._1

  /** Runs a statement that the database must reject, returning the server's message. */
  def rejectedSql(statement: String): IO[String] =
    sessions.use(_.execute(sql"#$statement".command))
      .attempt
      .map {
        case Left(e: skunk.exception.PostgresErrorException) => e.message
        case other                                           => fail(s"expected the database to reject: $statement, got $other")
      }

  /** A single bigint from a query. */
  def scalar(query: String): IO[Long] =
    sessions.use(_.unique(sql"#$query".query(skunk.codec.all.int8)))

  /** Unwraps an expected success, failing the test with the domain error otherwise. */
  def ok[E, A](io: IO[Either[E, A]]): IO[A] =
    io.flatMap {
      case Right(a)  => IO.pure(a)
      case Left(err) => IO.raiseError(new AssertionError(s"unexpected domain error: $err"))
    }

  /** Expects a specific failure. */
  def failsWith[E, A](io: IO[Either[E, A]])(expected: PartialFunction[E, Unit]): IO[Unit] =
    io.map {
      case Left(err) if expected.isDefinedAt(err) => ()
      case other                                  => fail(s"expected a matching domain error, got $other")
    }
}
