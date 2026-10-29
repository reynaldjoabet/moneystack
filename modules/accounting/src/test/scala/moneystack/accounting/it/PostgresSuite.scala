package moneystack.accounting.it

import moneystack.domain.*
import moneystack.accounting.AccountingModule

import cats.effect.*
import skunk.Session

import java.util.UUID

/** A throwaway Postgres per suite, with the accounting schema applied and the system chart bootstrapped. */
abstract class PostgresSuite extends moneystack.testkit.PostgresSuite[AccountingModule[IO]] {

  def schemas: List[String] = List("db/accounting.sql")

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, AccountingModule[IO]] =
    Resource.eval(IO(AccountingModule.make[IO](pool, CurrencyCode.USD)).flatTap(_.bootstrap))

  def books: AccountingModule[IO] = env

  /** The acting accountant; the module stores user ids without a users table. */
  val accountant: UserId = UserId.assume(UUID.randomUUID())
}
