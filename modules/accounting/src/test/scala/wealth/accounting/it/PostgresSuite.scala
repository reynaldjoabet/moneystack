package wealth.accounting.it

import wealth.domain.*
import wealth.accounting.AccountingModule
import wealth.accounting.domain.LedgerError

import cats.effect.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.testcontainers.postgresql.PostgreSQLContainer
import org.testcontainers.utility.{DockerImageName, MountableFile}
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider
import skunk.Session
import skunk.implicits.*

import java.util.UUID
import scala.concurrent.duration.*

/** A throwaway Postgres per suite, with the accounting schema applied and the system chart bootstrapped. */
object Postgres {

  private given TracerProvider[IO] = TracerProvider.noop[IO]
  private given MeterProvider[IO]  = MeterProvider.noop[IO]

  final case class Env(pool: Resource[IO, Session[IO]], books: AccountingModule[IO])

  private val container: Resource[IO, PostgreSQLContainer] =
    Resource.make(IO.blocking {
      val c = new PostgreSQLContainer(DockerImageName.parse("postgres:18-alpine"))
      c.start()
      c
    })(c => IO.blocking(c.stop()))

  /** Runs the schema through psql inside the container, as a deployment would. */
  private def applySchema(c: PostgreSQLContainer): IO[Unit] = IO.blocking {
    c.copyFileToContainer(MountableFile.forClasspathResource("db/accounting.sql"), "/tmp/accounting.sql")
    val r = c.execInContainer("psql", "-v", "ON_ERROR_STOP=1", "-q", "-U", c.getUsername, "-d", c.getDatabaseName, "-f", "/tmp/accounting.sql")
    if (r.getExitCode != 0) throw new IllegalStateException(s"accounting.sql failed: ${r.getStderr}")
  }

  def env(baseCurrency: CurrencyCode): Resource[IO, Env] =
    for {
      c     <- container
      _     <- Resource.eval(applySchema(c) *> applySchema(c))   // twice: the schema must be idempotent
      pool  <- Session.Builder[IO]
                 .withHost(c.getHost)
                 .withPort(c.getMappedPort(5432).intValue)
                 .withUserAndPassword(c.getUsername, c.getPassword)
                 .withDatabase(c.getDatabaseName)
                 .pooled(8)
      books  = AccountingModule.make[IO](pool, baseCurrency)
      _     <- Resource.eval(books.bootstrap)
    } yield Env(pool, books)
}

abstract class PostgresSuite extends CatsEffectSuite {

  override def munitIOTimeout: Duration = 3.minutes

  val db = ResourceSuiteLocalFixture("postgres", Postgres.env(CurrencyCode.USD))

  override def munitFixtures = List(db)

  def books: AccountingModule[IO] = db().books

  /** The acting accountant; the module stores user ids without a users table. */
  val accountant: UserId = UserId.assume(UUID.randomUUID())

  /** Unwraps an expected success, failing the test with the ledger error otherwise. */
  def ok[A](io: IO[Either[LedgerError, A]]): IO[A] =
    io.flatMap(_.leftMap(err => new AssertionError(s"unexpected ledger error: $err")).liftTo[IO])

  /** Expects a specific failure. */
  def failsWith[A](io: IO[Either[LedgerError, A]])(expected: PartialFunction[LedgerError, Unit]): IO[Unit] =
    io.map {
      case Left(err) if expected.isDefinedAt(err) => ()
      case other                                  => fail(s"expected a matching ledger error, got $other")
    }

  /** Runs a statement that must be rejected by the database, returning the server's message. */
  def rejectedSql(statement: String): IO[String] =
    db().pool.use(_.execute(sql"#$statement".command))
      .attempt
      .map {
        case Left(e: skunk.exception.PostgresErrorException) => e.message
        case other                                           => fail(s"expected the database to reject: $statement, got $other")
      }

  /** A single bigint from a query. */
  def scalar(query: String): IO[Long] =
    db().pool.use(_.unique(sql"#$query".query(skunk.codec.all.int8)))
}
