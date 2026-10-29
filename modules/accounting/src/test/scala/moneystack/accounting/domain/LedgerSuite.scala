package moneystack.accounting.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

class LedgerSuite extends munit.FunSuite {

  private val usd = CurrencyCode.USD
  private val eur = CurrencyCode.EUR

  private def account(code: String, t: AccountType, currency: CurrencyCode = usd, active: Boolean = true): Account =
    Account(
      AccountId.assume(UUID.randomUUID()),
      AccountCode.applyUnsafe(code),
      Title.applyUnsafe(s"acct $code"),
      t,
      currency,
      None,
      active,
      Instant.EPOCH
    )

  private def line(a: Account, side: Side, minor: Long): JournalLine =
    JournalLine(a.id, side, PriceMinor.applyUnsafe(minor), None)

  private val cash    = account("1000", AccountType.Cash)
  private val revenue = account("4000", AccountType.Income)
  private val fees    = account("5000", AccountType.CostOfSales)
  private val eurBank = account("1020", AccountType.Bank, currency = eur)
  private val closed  = account("1999", AccountType.Cash, active = false)

  private val chart = List(cash, revenue, fees, eurBank, closed).map(a => a.id -> a).toMap

  private def base(lines: List[PostedLine], side: Side): Long =
    lines.filter(_.side == side).map(_.baseAmountMinor.value: Long).sum

  // ---- validation ----

  test("balanced entry on active accounts is valid") {
    val lines = List(line(cash, Side.Debit, 1000), line(revenue, Side.Credit, 1000))
    assertEquals(Ledger.validate(lines, usd, usd, chart), Right(()))
  }

  test("needs at least two lines") {
    assertEquals(Ledger.validate(List(line(cash, Side.Debit, 1)), usd, usd, chart), Left(LedgerError.TooFewLines))
  }

  test("rejects unbalanced entries") {
    val lines = List(line(cash, Side.Debit, 1000), line(revenue, Side.Credit, 999))
    assertEquals(Ledger.validate(lines, usd, usd, chart), Left(LedgerError.Unbalanced(1000, 999)))
  }

  test("rejects unknown and inactive accounts") {
    val ghost = account("9999", AccountType.Cash)
    assertEquals(
      Ledger.validate(List(line(ghost, Side.Debit, 5), line(revenue, Side.Credit, 5)), usd, usd, chart),
      Left(LedgerError.UnknownAccount(ghost.id))
    )
    assertEquals(
      Ledger.validate(List(line(closed, Side.Debit, 5), line(revenue, Side.Credit, 5)), usd, usd, chart),
      Left(LedgerError.InactiveAccount(closed.code))
    )
  }

  test("foreign-currency account only takes entries in its own currency") {
    val lines = List(line(eurBank, Side.Debit, 100), line(revenue, Side.Credit, 100))
    assertEquals(Ledger.validate(lines, eur, usd, chart), Right(()))
    assertEquals(Ledger.validate(lines, usd, usd, chart), Left(LedgerError.CurrencyMismatch(eurBank.code, eur, usd)))
  }

  // ---- base-currency conversion ----

  test("rate of one keeps amounts") {
    val posted = Ledger.toBase(List(line(cash, Side.Debit, 1234), line(revenue, Side.Credit, 1234)), ExchangeRate.One)
    assertEquals(posted.map(_.baseAmountMinor.value: Long), List(1234L, 1234L))
    assertEquals(posted.map(_.lineNo), List(1, 2))
  }

  test("rounding drift is absorbed so the entry balances in base currency") {
    // Three credits of 1 at 1.5: each rounds half-even to 2 (sum 6), but 3 * 1.5 = 4.5 -> 4.
    val rate  = ExchangeRate.assume(BigDecimal("1.5"))
    val lines = List(line(cash, Side.Debit, 3), line(revenue, Side.Credit, 1), line(revenue, Side.Credit, 1), line(revenue, Side.Credit, 1))
    val posted = Ledger.toBase(lines, rate)
    assertEquals(base(posted, Side.Debit), 4L)
    assertEquals(base(posted, Side.Credit), 4L)
    assert(posted.forall(_.baseAmountMinor.value >= 0L))
  }

  test("balances across many awkward rates") {
    val rates = List("0.0137", "1.0833", "91.7724", "0.9999999", "3.3333333333").map(r => ExchangeRate.assume(BigDecimal(r)))
    val lines = List(
      line(cash, Side.Debit, 1001),
      line(fees, Side.Debit, 333),
      line(fees, Side.Debit, 7),
      line(revenue, Side.Credit, 777),
      line(revenue, Side.Credit, 563),
      line(revenue, Side.Credit, 1)
    )
    rates.foreach { r =>
      val posted = Ledger.toBase(lines, r)
      assertEquals(base(posted, Side.Debit), base(posted, Side.Credit), s"rate $r")
    }
  }

  test("a reversal at the same rate cancels exactly") {
    val rate   = ExchangeRate.assume(BigDecimal("1.0833"))
    val posted = Ledger.toBase(List(line(eurBank, Side.Debit, 999), line(revenue, Side.Credit, 333), line(revenue, Side.Credit, 666)), rate)
    val header = JournalHeader(
      JournalEntryId.assume(UUID.randomUUID()),
      LocalDate.of(2026, 1, 5),
      None,
      None,
      eur,
      rate,
      JournalSource.Manual,
      None,
      None,
      Instant.EPOCH
    )
    val undo = Ledger.toBase(Ledger.reversalOf(JournalEntry(header, posted)), rate)

    val net = (posted ++ undo).groupMapReduce(_.accountId)(l =>
      if (l.side == Side.Debit) l.baseAmountMinor.value: Long else -(l.baseAmountMinor.value: Long)
    )(_ + _)
    assert(net.values.forall(_ == 0L), net.toString)
  }

  // ---- normal balances & locking ----

  test("balance follows the account's normal side") {
    assertEquals(Ledger.balance(AccountType.Cash, 500, 200), 300L)
    assertEquals(Ledger.balance(AccountType.Income, 200, 500), 300L)
    assertEquals(Ledger.balance(AccountType.Expense, 50, 0), 50L)
    assertEquals(Ledger.balance(AccountType.AccountsPayable, 0, 80), 80L)
  }

  test("lock covers dates through lockThrough except a reopened window") {
    val by   = UserId.assume(UUID.randomUUID())
    val lock = LedgerLock(
      LocalDate.of(2026, 3, 31),
      Some(LocalDate.of(2026, 2, 1)),
      Some(LocalDate.of(2026, 2, 28)),
      Body.applyUnsafe("Q1 close"),
      by,
      Instant.EPOCH
    )
    assert(lock.locks(LocalDate.of(2026, 1, 15)))
    assert(!lock.locks(LocalDate.of(2026, 2, 14)))
    assert(lock.locks(LocalDate.of(2026, 3, 31)))
    assert(!lock.locks(LocalDate.of(2026, 4, 1)))
    assert(lock.copy(unlockFrom = None, unlockTo = None).locks(LocalDate.of(2026, 2, 14)))
  }
}
