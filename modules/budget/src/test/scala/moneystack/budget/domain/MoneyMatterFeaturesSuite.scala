package moneystack.budget.domain

import moneystack.domain.*
import moneystack.platform.FxTable
import Fixtures.*

import io.circe.syntax.*

import java.util.UUID

/** Excluding transactions from stats, budget pace, rollover resets, period notes and tag analysis. */
class MoneyMatterFeaturesSuite extends munit.FunSuite {

  private val usd      = CurrencyCode.USD
  private val checking = wallet()
  private val wallets  = Map(checking.id -> checking)
  private val food     = category("Food")
  private val cats     = List(food)

  private def entries(txs: List[Transaction]) = Reports.entries(txs, wallets, FxTable.empty, usd).toOption.get

  test("an excluded transaction, and a refund of it, leave reports but not balances") {
    val car    = tx(checking, TxKind.Income, 900_000, d(3, 2), title = "Sold the car").copy(excludeFromStats = true)
    val laptop = tx(checking, TxKind.Expense, 150_000, d(3, 3), Some(food)).copy(excludeFromStats = true)
    val refund = tx(checking, TxKind.Income, 20_000, d(3, 9), refundOf = Some(laptop))
    val lunch  = tx(checking, TxKind.Expense, 1_500, d(3, 4), Some(food))
    val txs    = List(car, laptop, refund, lunch)
    val s      = Reports.summary(entries(txs), Period(d(3, 1), d(3, 31)), usd)
    assertEquals((s.incomeMinor, s.expenseMinor, s.transactions), (0L, 1_500L, 1))
    val moved = Reports.balances(List(checking), txs, d(3, 31), FxTable.empty, usd).toOption.get
    assertEquals(moved.map(_.balanceMinor), List(900_000L - 150_000 + 20_000 - 1_500), "balances still count all of them")
  }

  private val monthly = budget(Recurrence.Monthly, d(1, 1), amount = 30_000).copy(categoryIds = List(food.id))

  test("pace follows the usual share spent by this point of the period") {
    // January and February: half the month's spending by day 10.
    val history = List(1, 2).flatMap(m =>
      List(tx(checking, TxKind.Expense, 10_000, d(m, 5), Some(food)), tx(checking, TxKind.Expense, 10_000, d(m, 20), Some(food)))
    )
    val march = List(tx(checking, TxKind.Expense, 20_000, d(3, 4), Some(food)))
    val st    = Reports.status(monthly, entries(history ++ march), cats, d(3, 10)).get
    val pace  = st.pace.get
    assertEquals(pace.source, "history")
    assertEquals(pace.expectedPct, BigDecimal("50.00"))
    assertEquals(pace.expectedMinor, 15_000L, "half of the 30,000 available")
    assertEquals((pace.ahead, pace.aheadMinor), (true, 5_000L))
  }

  test("without two earlier periods of spending, pace is the share of days elapsed") {
    val st   = Reports.status(monthly, entries(List(tx(checking, TxKind.Expense, 1_000, d(3, 1), Some(food)))), cats, d(3, 15)).get
    val pace = st.pace.get
    assertEquals(pace.source, "even")
    assertEquals(pace.expectedPct, BigDecimal("48.39"), "15 of March's 31 days")
    assertEquals(pace.ahead, false)
  }

  test("a reset period starts without the previous one's rollover, and shows its note") {
    val rolling = monthly.copy(rollover = true)
    val feb     = entries(List(tx(checking, TxKind.Expense, 10_000, d(2, 10), Some(food))))
    assertEquals(Reports.status(rolling, feb, cats, d(3, 10)).get.rolloverMinor, 20_000L)
    val mark = BudgetPeriodMark(rolling.id, d(3, 1), Some("Holiday month"), rolloverReset = true)
    val st   = Reports.status(rolling, feb, cats, d(3, 10), Map(d(3, 1) -> mark)).get
    assertEquals((st.rolloverMinor, st.availableMinor, st.rolloverReset, st.note), (0L, 30_000L, true, Some("Holiday month")))
  }

  test("tag analysis counts a transaction under each of its tags, against all tagged spending and income") {
    val trip = Tag(TagId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Trip"))
    val work = Tag(TagId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Work"))
    val txs  = List(
      tx(checking, TxKind.Income, 100_000, d(3, 1)),
      tx(checking, TxKind.Expense, 6_000, d(3, 2), Some(food)).copy(tags = List(trip.id)),
      tx(checking, TxKind.Expense, 4_000, d(3, 3), Some(food)).copy(tags = List(trip.id, work.id)),
      tx(checking, TxKind.Expense, 9_000, d(3, 4), Some(food))
    )
    val totals = Reports.byTag(entries(txs), Period(d(3, 1), d(3, 31)), List(trip, work))
    assertEquals(
      totals.map(t => (t.name, t.amountMinor, t.pctOfTags, t.pctOfIncome)),
      List(
        ("Trip", 10_000L, BigDecimal("71.43"), Some(BigDecimal("10.00"))),
        ("Work", 4_000L, BigDecimal("28.57"), Some(BigDecimal("4.00")))
      )
    )
  }

  test("a rule can exclude from stats, and the action survives JSON") {
    import RuleJson.given
    val rule = Rule(
      RuleId.assume(UUID.randomUUID()),
      owner,
      Title.applyUnsafe("Car sale"),
      1,
      enabled = true,
      matchAll = true,
      List(Condition.Text(TextField.Title, TextOp.ContainsAny, List("car"))),
      List(Action.ExcludeFromStats),
      0L,
      java.time.Instant.EPOCH
    )
    assert(Rules.run(List(rule), tx(checking, TxKind.Income, 1, d(3, 2), title = "Sold the car"))._1.excludeFromStats)
    assert(!Rules.run(List(rule), tx(checking, TxKind.Income, 1, d(3, 2), title = "Salary"))._1.excludeFromStats)
    assertEquals((Action.ExcludeFromStats: Action).asJson.as[Action], Right(Action.ExcludeFromStats))
    assert(Rules.validate(rule.conditions, List(Action.ExcludeFromStats, Action.ExcludeFromStats)).isLeft)
  }

  test("without history, pace puts scheduled payments on their dates and spreads the rest evenly") {
    val rent =
      schedule(Frequency.Monthly, d(1, 1)).copy(walletId = checking.id, amountMinor = 20_000, categoryId = Some(food.id), nextIndex = 3)
    // March 20 is behind nextIndex without a payment, so it was skipped.
    val gym =
      schedule(Frequency.Monthly, d(1, 20)).copy(walletId = checking.id, amountMinor = 5_000, categoryId = Some(food.id), nextIndex = 3)
    val tv    = schedule(Frequency.Monthly, d(1, 25)).copy(walletId = checking.id, amountMinor = 1_500)
    val pay   = schedule(Frequency.Monthly, d(1, 28)).copy(walletId = checking.id, txKind = TxKind.Income, categoryId = Some(food.id))
    val march = Period(d(3, 1), d(3, 31))
    val paid  = Set(rent.id -> d(3, 1))
    val plan  = Reports.planned(monthly, List(rent, gym, tv, pay), paid, march, cats, wallets, FxTable.empty).toOption.get
    assertEquals(plan, List(d(3, 1) -> 20_000L), "only the rent: the gym was skipped, TV is another category, pay is income")

    val spent = entries(List(tx(checking, TxKind.Expense, 21_000, d(3, 1), Some(food))))
    val pace  = Reports.status(monthly, spent, cats, d(3, 10), planned = plan).get.pace.get
    // (20,000 due + 10/31 of the other 10,000) / 30,000
    assertEquals((pace.source, pace.expectedPct, pace.expectedMinor, pace.ahead), ("schedule", BigDecimal("77.42"), 23_226L, false))

    val unpaid = Reports.planned(monthly, List(rent), Set.empty, march, cats, wallets, FxTable.empty).toOption.get
    assertEquals(unpaid, Nil, "an occurrence behind nextIndex without a payment was skipped")
    val upcoming = Reports.planned(monthly, List(rent.copy(nextIndex = 2)), Set.empty, march, cats, wallets, FxTable.empty).toOption.get
    assertEquals(upcoming, List(d(3, 1) -> 20_000L), "an unpaid one still ahead counts")
    assertEquals(
      Reports.planned(monthly, List(rent.copy(nextIndex = 2, active = false)), Set.empty, march, cats, wallets, FxTable.empty),
      Right(Nil),
      "a paused schedule's unpaid ones don't"
    )
  }

  test("tag targets compare the share of income; tagged transfers count when asked") {
    val savings = wallet(kind = WalletKind.Savings)
    val both    = wallets + (savings.id -> savings)
    val save    = Tag(TagId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Savings"), Some(BigDecimal(20)))
    val fun     = Tag(TagId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Fun"), Some(BigDecimal(10)))
    val txs     = List(
      tx(checking, TxKind.Income, 100_000, d(3, 1)),
      tx(checking, TxKind.Expense, 12_000, d(3, 2), Some(food)).copy(tags = List(fun.id)),
      tx(checking, TxKind.Transfer, 15_000, d(3, 3), to = Some(savings)).copy(tags = List(save.id)),
      tx(savings, TxKind.Transfer, 1_000, d(3, 4), to = Some(checking)).copy(tags = List(save.id), excludeFromStats = true)
    )
    val es    = Reports.entries(txs, both, FxTable.empty, usd).toOption.get
    val moved = Reports.transferEntries(txs, both, FxTable.empty, usd).toOption.get
    val march = Period(d(3, 1), d(3, 31))
    val shown = (xs: List[TagTotal]) => xs.map(t => (t.name, t.amountMinor, t.targetPct, t.overTarget))
    assertEquals(
      shown(Reports.byTag(es, march, List(save, fun))),
      List(("Fun", 12_000L, Some(BigDecimal(10)), Some(true)), ("Savings", 0L, Some(BigDecimal(20)), Some(false)))
    )
    assertEquals(
      shown(Reports.byTag(es ++ moved, march, List(save, fun))),
      List(("Savings", 15_000L, Some(BigDecimal(20)), Some(false)), ("Fun", 12_000L, Some(BigDecimal(10)), Some(true))),
      "the transfer to savings counts once; the excluded one not at all"
    )
  }
}
