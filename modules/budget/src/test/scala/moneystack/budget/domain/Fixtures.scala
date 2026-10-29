package moneystack.budget.domain

import moneystack.domain.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/** Builders for the domain tests. */
object Fixtures {

  val owner: UserId                                 = UserId.assume(UUID.randomUUID())
  def d(m: Int, day: Int, y: Int = 2026): LocalDate = LocalDate.of(y, m, day)

  def wallet(ccy: CurrencyCode = CurrencyCode.USD, opening: Long = 0L, kind: WalletKind = WalletKind.Checking): Wallet =
    Wallet(WalletId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Wallet"), kind, ccy, opening, archived = false, Instant.EPOCH)

  def category(name: String, kind: CategoryKind = CategoryKind.Expense, parent: Option[Category] = None): Category =
    Category(CategoryId.assume(UUID.randomUUID()), owner, Title.applyUnsafe(name), kind, parent.map(_.id), None, archived = false)

  def tx(
      w: Wallet,
      kind: TxKind,
      amount: Long,
      date: LocalDate,
      cat: Option[Category] = None,
      title: String = "tx",
      splits: List[Split] = Nil,
      to: Option[Wallet] = None,
      toAmount: Option[Long] = None,
      refundOf: Option[Transaction] = None,
      objective: Option[ObjectiveId] = None
  ): Transaction =
    Transaction(
      TransactionId.assume(UUID.randomUUID()),
      owner,
      w.id,
      kind,
      amount,
      date,
      Title.applyUnsafe(title),
      None,
      cat.map(_.id),
      if (cat.isDefined) CategorySource.Manual else CategorySource.Unset,
      None,
      Nil,
      splits,
      to.map(_.id),
      toAmount,
      refundOf.map(_.id),
      objective,
      None,
      None,
      Nil,
      None,
      owner,
      Instant.EPOCH
    )

  def budget(recurrence: Recurrence, start: LocalDate, amount: Long = 1000L, length: Int = 1, end: Option[LocalDate] = None): Budget =
    Budget(
      BudgetId.assume(UUID.randomUUID()),
      owner,
      Title.applyUnsafe("Budget"),
      amount,
      CurrencyCode.USD,
      recurrence,
      length,
      start,
      end,
      Nil,
      Nil,
      Nil,
      income = false,
      rollover = false,
      Nil,
      pinned = false,
      archived = false,
      Instant.EPOCH
    )

  def schedule(freq: Frequency, start: LocalDate, interval: Int = 1, max: Option[Int] = None, end: Option[LocalDate] = None): Schedule =
    Schedule(
      ScheduleId.assume(UUID.randomUUID()),
      owner,
      ScheduleKind.Subscription,
      Title.applyUnsafe("Netflix"),
      WalletId.assume(UUID.randomUUID()),
      TxKind.Expense,
      1599L,
      None,
      None,
      None,
      freq,
      interval,
      start,
      end,
      max,
      autoPay = false,
      3,
      0,
      active = true,
      Instant.EPOCH
    )
}
