package moneystack.budget.domain

import moneystack.domain.*
import moneystack.platform.{FxQuote, FxTable}
import Fixtures.*

import java.time.{Instant, YearMonth}
import java.util.UUID

class ReportsSuite extends munit.FunSuite {

  private val usd      = CurrencyCode.USD
  private val eur      = CurrencyCode.EUR
  private val checking = wallet(opening = 100_000)
  private val euros    = wallet(eur)
  private val card     = wallet(kind = WalletKind.CreditCard)

  private val food      = category("Food")
  private val groceries = category("Groceries", parent = Some(food))
  private val dining    = category("Dining", parent = Some(food))
  private val rent      = category("Rent")
  private val salary    = category("Salary", CategoryKind.Income)
  private val cats      = List(food, groceries, dining, rent, salary)

  private val fx      = FxTable(List(FxQuote(eur, usd, d(1, 1), BigDecimal("1.10"))))
  private val wallets = List(checking, euros, card).map(w => w.id -> w).toMap

  private val shop = tx(checking, TxKind.Expense, 10_000, d(3, 3), Some(groceries))
  private val txs  = List(
    tx(checking, TxKind.Income, 300_000, d(3, 1), Some(salary)),
    shop,
    tx(checking, TxKind.Expense, 120_000, d(3, 5), Some(rent)),
    tx(euros, TxKind.Expense, 5_000, d(3, 6), Some(dining)), // 55.00 USD
    tx(checking, TxKind.Expense, 8_000, d(3, 7), splits = List(Split(groceries.id, 6_000, None), Split(dining.id, 2_000, None))),
    tx(checking, TxKind.Income, 2_500, d(3, 8), refundOf = Some(shop)), // refund lowers groceries
    tx(checking, TxKind.Transfer, 50_000, d(3, 9), to = Some(card)),
    tx(checking, TxKind.Expense, 9_000, d(2, 10), Some(groceries))
  )

  private val march = Period(d(3, 1), d(3, 31))
  private val es    = Reports.entries(txs, wallets, fx, usd).toOption.get

  test("entries: splits, refunds as negative spending, foreign wallets converted, transfers left out") {
    assertEquals(es.filter(_.kind == CategoryKind.Expense).map(_.amountMinor).sum, 10_000L + 120_000 + 5_500 + 8_000 - 2_500 + 9_000)
    assertEquals(es.count(_.txId == txs(4).id), 2)
    assertEquals(
      es.find(_.txId == txs(5).id).map(e => (e.kind, e.categoryId, e.amountMinor)),
      Some((CategoryKind.Expense, Some(groceries.id), -2_500L))
    )
  }

  test("summary and savings rate") {
    val s = Reports.summary(es, march, usd)
    assertEquals(s.incomeMinor, 300_000L)
    assertEquals(s.expenseMinor, 141_000L)
    assertEquals(s.netMinor, 159_000L)
    assertEquals(s.savingsRatePct, Some(BigDecimal("53.00")))
  }

  test("categories roll subcategories into their parent") {
    val bc = Reports.byCategory(es, march, CategoryKind.Expense, cats)
    assertEquals(bc.map(c => (c.name, c.amountMinor)), List("Rent" -> 120_000L, "Food" -> 21_000L))
    assertEquals(bc(1).subcategories.map(c => (c.name, c.amountMinor)), List("Groceries" -> 13_500L, "Dining" -> 7_500L))
  }

  test("trend buckets include empty ones; comparison is against the previous period") {
    val t = Reports.trend(es, Period(d(2, 1), d(4, 30)), Bucket.Month)
    assertEquals(t.map(_.expenseMinor), List(9_000L, 141_000L, 0L))
    val cmp = Reports.compare(es, Period(d(3, 1), d(3, 28)), cats)
    assert(cmp.exists(c => c.name == "Food" && c.previousMinor == 9_000L))
  }

  test("cash flow links income through to spending and savings") {
    val links = Reports.cashflow(es, march, cats)
    assert(links.contains(FlowLink("Salary", "Income", 300_000L)))
    assert(links.contains(FlowLink("Income", "Savings", 159_000L)))
    assertEquals(links.filter(_.source == "Income").map(_.valueMinor).sum, 300_000L)
  }

  test("balances apply opening balances, transfers and currency") {
    val bs = Reports.balances(List(checking, euros, card), txs, d(3, 31), fx, usd).toOption.get.map(b => b.walletId -> b).toMap
    assertEquals(bs(checking.id).balanceMinor, 100_000L + 300_000 - 10_000 - 120_000 - 8_000 + 2_500 - 50_000 - 9_000)
    assertEquals(bs(card.id).balanceMinor, 50_000L)
    assertEquals(bs(euros.id).baseMinor, -5_500L)
    assert(Reports.balances(List(euros), txs, d(3, 31), FxTable.empty, usd).isLeft)
  }

  test("budget status with filters, limits and rollover") {
    val b = budget(Recurrence.Monthly, d(1, 1), amount = 30_000).copy(
      categoryIds = List(food.id),
      rollover = true,
      categoryLimits = List(CategoryLimit(food.id, 25_000))
    )
    val st = Reports.status(b, es, cats, d(3, 16)).get
    assertEquals(st.period, march)
    assertEquals(st.spentMinor, 21_000L)    // food incl. subcategories, net of the refund
    assertEquals(st.rolloverMinor, 21_000L) // February: 30,000 - 9,000
    assertEquals(st.availableMinor, 51_000L)
    assertEquals(st.remainingMinor, 30_000L)
    assertEquals(st.daysLeft, 16L)
    assertEquals(st.dailyAllowanceMinor, 1_875L)
    assertEquals(st.categories.map(c => (c.name, c.limitMinor, c.spentMinor)), List(("Food", Some(25_000L), 21_000L)))
    val excludedRent = budget(Recurrence.Monthly, d(1, 1)).copy(excludedCategoryIds = List(rent.id), walletIds = List(checking.id))
    assertEquals(Reports.status(excludedRent, es, cats, d(3, 16)).get.spentMinor, 10_000L + 8_000 - 2_500)
  }

  test("objectives: goals add contributions, loans track what is outstanding") {
    def obj(kind: ObjectiveKind) =
      Objective(ObjectiveId.assume(UUID.randomUUID()), owner, kind, Title.applyUnsafe("o"), 100_000L, usd, None, None, false, Instant.EPOCH)
    val goal =
      Reports.objective(obj(ObjectiveKind.Goal), List(TxKind.Expense -> 30_000L, TxKind.Transfer -> 20_000L, TxKind.Income -> 5_000L))
    assertEquals((goal.amountMinor, goal.remainingMinor, goal.pct), (45_000L, 55_000L, BigDecimal("45.00")))
    val lent = Reports.objective(obj(ObjectiveKind.Lent), List(TxKind.Expense -> 40_000L, TxKind.Income -> 10_000L))
    assertEquals((lent.principalMinor, lent.repaidMinor, lent.amountMinor, lent.pct), (40_000L, 10_000L, 30_000L, BigDecimal("25.00")))
    val borrowed = Reports.objective(obj(ObjectiveKind.Borrowed), List(TxKind.Income -> 40_000L, TxKind.Expense -> 40_000L))
    assertEquals((borrowed.amountMinor, borrowed.pct), (0L, BigDecimal("100.00")))
  }

  test("streaks and monthly recap") {
    val s = Reports.streak(Set(d(3, 1), d(3, 2), d(3, 3), d(3, 5), d(3, 6)), d(3, 7))
    assertEquals(s, Streak(2, 3, loggedToday = false))
    val r = Reports.recap(es, YearMonth.of(2026, 3), cats, usd)
    assertEquals(r.previousExpenseMinor, 9_000L)
    assertEquals(r.largestExpense, Some(txs(2).id))
    assertEquals(r.topCategories.head.name, "Rent")
  }
}
