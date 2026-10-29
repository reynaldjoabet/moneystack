package moneystack.accounting.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

class BankRulesSuite extends munit.FunSuite {

  private val bank    = AccountId.assume(UUID.randomUUID())
  private val expense = AccountId.assume(UUID.randomUUID())

  private def tx(description: String, amount: Long): BankTransaction =
    BankTransaction(
      BankTransactionId.assume(UUID.randomUUID()),
      bank,
      LocalDate.of(2026, 3, 1),
      Title.applyUnsafe(description),
      amount,
      None,
      BankTxStatus.Uncategorized,
      None,
      None,
      None,
      None,
      None,
      Instant.EPOCH
    )

  private def rule(
      conditions: List[RuleCondition],
      applyTo: RuleApplyTo = RuleApplyTo.Both,
      conjunction: Conjunction = Conjunction.All,
      priority: Int = 100,
      active: Boolean = true,
      createdAt: Instant = Instant.EPOCH
  ): BankRule =
    BankRule(
      BankRuleId.assume(UUID.randomUUID()),
      Title.applyUnsafe("rule"),
      priority,
      applyTo,
      conjunction,
      conditions,
      expense,
      None,
      autoCategorize = true,
      active,
      createdAt
    )

  private def cond(field: RuleField, op: RuleOperator, value: String) = RuleCondition(field, op, value)

  test("validation rejects empty, blank and mismatched conditions") {
    assert(BankRules.validate(Nil).isLeft)
    assert(BankRules.validate(List(cond(RuleField.Description, RuleOperator.Contains, "  "))).isLeft)
    assert(BankRules.validate(List(cond(RuleField.Amount, RuleOperator.Contains, "10"))).isLeft)
    assert(BankRules.validate(List(cond(RuleField.Amount, RuleOperator.GreaterThan, "ten"))).isLeft)
    assert(BankRules.validate(List(cond(RuleField.Description, RuleOperator.LessThan, "x"))).isLeft)
    assertEquals(
      BankRules.validate(List(
        cond(RuleField.Amount, RuleOperator.GreaterThan, "1000"),
        cond(RuleField.Description, RuleOperator.StartsWith, "AWS")
      )),
      Right(())
    )
  }

  test("description matching is case-insensitive") {
    val aws = rule(List(cond(RuleField.Description, RuleOperator.Contains, "amazon web")))
    assert(BankRules.matches(aws, tx("AMAZON WEB SERVICES EMEA", -12_000)))
    assert(!BankRules.matches(aws, tx("Amazon Marketplace", -12_000)))
    assert(BankRules.matches(rule(List(cond(RuleField.Description, RuleOperator.EndsWith, "emea"))), tx("AWS EMEA", -1)))
    assert(BankRules.matches(rule(List(cond(RuleField.Description, RuleOperator.Equals, "rent"))), tx("RENT", -1)))
  }

  test("amount conditions compare the absolute amount, direction is separate") {
    val big = rule(List(cond(RuleField.Amount, RuleOperator.GreaterThan, "10000")), applyTo = RuleApplyTo.Withdrawals)
    assert(BankRules.matches(big, tx("x", -15_000)))
    assert(!BankRules.matches(big, tx("x", 15_000)))
    assert(!BankRules.matches(big, tx("x", -10_000)))
    assert(BankRules.matches(rule(List(cond(RuleField.Amount, RuleOperator.Equals, "500"))), tx("x", 500)))
  }

  test("all vs any") {
    val conds = List(cond(RuleField.Description, RuleOperator.Contains, "stripe"), cond(RuleField.Amount, RuleOperator.LessThan, "100"))
    assert(!BankRules.matches(rule(conds), tx("STRIPE PAYOUT", 5_000)))
    assert(BankRules.matches(rule(conds, conjunction = Conjunction.Any), tx("STRIPE PAYOUT", 5_000)))
  }

  test("first match honours priority, then age, and skips inactive rules") {
    val c       = List(cond(RuleField.Description, RuleOperator.Contains, "uber"))
    val older   = rule(c, priority = 10, createdAt = Instant.EPOCH)
    val newer   = rule(c, priority = 10, createdAt = Instant.EPOCH.plusSeconds(5))
    val urgent  = rule(c, priority = 1, active = false)
    val generic = rule(c, priority = 50)
    assertEquals(BankRules.firstMatch(List(generic, newer, urgent, older), tx("UBER TRIP", -2_300)).map(_.id), Some(older.id))
    assertEquals(BankRules.firstMatch(List(generic), tx("LYFT", -2_300)), None)
  }
}
