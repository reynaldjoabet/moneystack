package moneystack.accounting.it

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.service.BankingService.{NewRule, NewTransaction}
import moneystack.accounting.service.LedgerService.SystemAccounts

import cats.effect.*

import java.time.LocalDate

class BankingIntegrationSuite extends PostgresSuite {

  private def d(m: Int, day: Int) = LocalDate.of(2026, m, day)
  private val bank                = SystemAccounts.PaymentClearing
  private val farFuture           = LocalDate.of(2030, 1, 1)

  private def row(day: Int, description: String, amount: Long, ext: String) =
    NewTransaction(d(7, day), Title.applyUnsafe(description), amount, Some(ext))

  private def rule(name: String, priority: Int, auto: Boolean, account: AccountId, conditions: RuleCondition*) =
    NewRule(Title.applyUnsafe(name), Some(priority), RuleApplyTo.Both, Conjunction.All, conditions.toList, account, None, auto)

  private def cond(field: RuleField, op: RuleOperator, value: String) = RuleCondition(field, op, value)

  private def feed: IO[List[BankTransaction]] = books.banking.transactions(Some(bank.id), None, 100)

  private def byExt(ext: String): IO[BankTransaction] = feed.map(_.find(_.externalId.contains(ext)).get)

  private def bankBalance: IO[Long] =
    books.ledger.trialBalance(farFuture).map(_.rows.find(_.accountId == bank.id).fold(0L)(r => (r.debitMinor.value: Long) - r.creditMinor.value))

  test("rules are validated against the chart and their conditions") {
    for {
      _ <- failsWith(books.banking.createRule(rule("empty", 1, auto = false, SystemAccounts.GeneralExpenses.id))) { case LedgerError.InvalidRule(_) => }
      _ <- failsWith(books.banking.createRule(rule("bad", 1, auto = false, AccountId.assume(java.util.UUID.randomUUID()),
                                                   cond(RuleField.Description, RuleOperator.Contains, "x")))) { case LedgerError.UnknownAccount(_) => }
    } yield ()
  }

  test("import de-duplicates, auto-categorises matching rules and tags suggestions") {
    for {
      _  <- ok(books.banking.createRule(rule("AWS", 10, auto = true, SystemAccounts.GeneralExpenses.id,
                                             cond(RuleField.Description, RuleOperator.Contains, "amazon web services"))))
      _  <- ok(books.banking.createRule(rule("Stripe payouts", 20, auto = false, SystemAccounts.Sales.id,
                                             cond(RuleField.Description, RuleOperator.StartsWith, "stripe"),
                                             cond(RuleField.Amount, RuleOperator.GreaterThan, "1000"))))
      r1 <- ok(books.banking.importTransactions(bank.id, List(
              row(1, "AMAZON WEB SERVICES EMEA", -12_000, "tx-1"),
              row(2, "STRIPE PAYOUT 8812", 50_000, "tx-2"),
              row(3, "Coffee", -450, "tx-3")
            ), accountant))
      r2 <- ok(books.banking.importTransactions(bank.id, List(row(1, "AMAZON WEB SERVICES EMEA", -12_000, "tx-1")), accountant))
      aws    <- byExt("tx-1")
      stripe <- byExt("tx-2")
      coffee <- byExt("tx-3")
      bal    <- bankBalance
    } yield {
      assertEquals((r1.imported, r1.duplicates, r1.matched, r1.categorized), (3, 0, 2, 1))
      assertEquals((r2.imported, r2.duplicates), (0, 1))
      assertEquals(aws.status, BankTxStatus.Categorized)
      assertEquals(aws.categoryAccountId, Some(SystemAccounts.GeneralExpenses.id))
      assertEquals(stripe.status, BankTxStatus.Uncategorized)
      assert(stripe.ruleId.isDefined)
      assertEquals(coffee.ruleId, None)
      assertEquals(bal, -12_000L)
    }
  }

  test("manual categorise, uncategorise, exclude and restore") {
    for {
      stripe <- byExt("tx-2")
      coffee <- byExt("tx-3")
      cat    <- ok(books.banking.categorize(stripe.id, SystemAccounts.Sales.id, None, accountant))
      _      <- failsWith(books.banking.categorize(stripe.id, SystemAccounts.Sales.id, None, accountant)) { case LedgerError.InvalidTransactionState("categorized", _) => }
      bal1   <- bankBalance
      unc    <- ok(books.banking.uncategorize(stripe.id, accountant))
      bal2   <- bankBalance
      exc    <- ok(books.banking.exclude(coffee.id))
      _      <- failsWith(books.banking.categorize(coffee.id, SystemAccounts.GeneralExpenses.id, None, accountant)) { case LedgerError.InvalidTransactionState("excluded", _) => }
      res    <- ok(books.banking.restore(coffee.id))
      _      <- failsWith(books.banking.restore(coffee.id)) { case LedgerError.InvalidTransactionState(_, "restore") => }
    } yield {
      assertEquals(cat.status, BankTxStatus.Categorized)
      assertEquals(bal1, -12_000L + 50_000L)
      assertEquals(unc.status, BankTxStatus.Uncategorized)
      assertEquals(unc.entryId, None)
      assertEquals(bal2, -12_000L)
      assertEquals(exc.status, BankTxStatus.Excluded)
      assertEquals(res.status, BankTxStatus.Uncategorized)
    }
  }

  test("running rules again picks up newly created rules") {
    for {
      _   <- ok(books.banking.createRule(rule("Coffee", 5, auto = true, SystemAccounts.GeneralExpenses.id,
                                              cond(RuleField.Description, RuleOperator.Equals, "coffee"))))
      res <- books.banking.runRules(accountant)
      cof <- byExt("tx-3")
    } yield {
      assertEquals(res.categorized, 1)
      assertEquals(cof.status, BankTxStatus.Categorized)
    }
  }

  test("only money accounts take bank feeds") {
    failsWith(books.banking.importTransactions(SystemAccounts.Sales.id, List(row(9, "x", 1, "tx-9")), accountant)) {
      case LedgerError.WrongAccountType(_, _) =>
    }
  }

  test("a categorisation in a locked period is reported, not lost") {
    val lock = LedgerLock(d(7, 31), None, None, Body.applyUnsafe("July close"), accountant, java.time.Instant.now())
    for {
      _   <- books.ledger.setLock(lock)
      res <- ok(books.banking.importTransactions(bank.id, List(row(20, "COFFEE", -300, "tx-20")), accountant))
      tx  <- byExt("tx-20")
      _   <- books.ledger.clearLock
    } yield {
      assertEquals((res.matched, res.categorized, res.failed.size), (1, 0, 1))
      assert(res.failed.head._2.contains("PeriodLocked"), res.failed.toString)
      assertEquals(tx.status, BankTxStatus.Uncategorized)
    }
  }
}
