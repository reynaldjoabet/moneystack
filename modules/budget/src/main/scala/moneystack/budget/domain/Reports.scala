package moneystack.budget.domain

import moneystack.domain.*
import moneystack.platform.FxTable

import java.time.{DayOfWeek, LocalDate, YearMonth}
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import scala.math.BigDecimal.RoundingMode

/**
 * Income or spending attributed to one category, in a reporting currency.
 * Splits become one entry each; a refund is negative spending in the
 * refunded category; transfers are not income or spending.
 */
final case class Entry(
    txId: TransactionId,
    date: LocalDate,
    walletId: WalletId,
    kind: CategoryKind,
    categoryId: Option[CategoryId],
    payeeId: Option[PayeeId],
    amountMinor: Long,
    excludedBudgets: List[BudgetId]
)

final case class Summary(from: LocalDate, to: LocalDate, currency: CurrencyCode, incomeMinor: Long, expenseMinor: Long, netMinor: Long,
                         savingsRatePct: Option[BigDecimal], averageDailyExpenseMinor: Long, transactions: Int)

final case class CategoryTotal(categoryId: Option[CategoryId], name: String, amountMinor: Long, pct: BigDecimal, subcategories: List[CategoryTotal])

enum Bucket(val key: String) {
  case Day   extends Bucket("day")
  case Week  extends Bucket("week")
  case Month extends Bucket("month")
}
object Bucket extends Keyed[Bucket] { def key(a: Bucket) = a.key }

final case class TrendPoint(start: LocalDate, incomeMinor: Long, expenseMinor: Long, netMinor: Long)

final case class CategoryChange(categoryId: Option[CategoryId], name: String, currentMinor: Long, previousMinor: Long, changeMinor: Long, changePct: Option[BigDecimal])

final case class FlowLink(source: String, target: String, valueMinor: Long)

final case class WalletBalance(walletId: WalletId, name: Title, kind: WalletKind, currency: CurrencyCode, balanceMinor: Long, baseMinor: Long)

final case class Streak(current: Int, longest: Int, loggedToday: Boolean)

final case class Recap(month: YearMonth, summary: Summary, topCategories: List[CategoryTotal], largestExpense: Option[TransactionId],
                       previousExpenseMinor: Long, expenseChangePct: Option[BigDecimal])

final case class LimitStatus(categoryId: CategoryId, name: String, limitMinor: Option[Long], spentMinor: Long)

/**
 * `availableMinor` is the amount plus any rollover; the daily allowance spreads
 * what's left over the remaining days; `projectedMinor` extrapolates the pace so far.
 */
final case class BudgetStatus(
    budgetId: BudgetId,
    name: Title,
    period: Period,
    currency: CurrencyCode,
    amountMinor: Long,
    rolloverMinor: Long,
    availableMinor: Long,
    spentMinor: Long,
    remainingMinor: Long,
    usedPct: BigDecimal,
    daysLeft: Long,
    dailyAllowanceMinor: Long,
    projectedMinor: Long,
    over: Boolean,
    categories: List[LimitStatus]
)

/** For goals `amountMinor` is saved; for loans it is outstanding. */
final case class ObjectiveProgress(objective: Objective, principalMinor: Long, repaidMinor: Long, amountMinor: Long,
                                   remainingMinor: Long, pct: BigDecimal, linked: Int)

object Reports {

  private def pct(part: BigDecimal, whole: BigDecimal): BigDecimal =
    if (whole == 0) BigDecimal(0) else (part / whole * 100).setScale(2, RoundingMode.HALF_EVEN)

  private def minor(x: BigDecimal): Long = x.setScale(0, RoundingMode.HALF_EVEN).toLong

  /** Converts minor units between currencies on a date. */
  def convert(fx: FxTable, amountMinor: Long, from: CurrencyCode, to: CurrencyCode, date: LocalDate): Either[BudgetError, Long] =
    fx.convertOpt(BigDecimal(amountMinor), from, to, date).map(minor).toRight(BudgetError.MissingFxRate(from, to))

  def entries(
      txs: List[Transaction],
      wallets: Map[WalletId, Wallet],
      fx: FxTable,
      currency: CurrencyCode
  ): Either[BudgetError, List[Entry]] = {
    val byId = txs.map(t => t.id -> t).toMap
    txs.filter(_.kind != TxKind.Transfer).foldLeft[Either[BudgetError, List[Entry]]](Right(Nil)) { (acc, t) =>
      val ccy = wallets.get(t.walletId).map(_.currency).getOrElse(currency)
      def conv(x: Long) = convert(fx, x, ccy, currency, t.date)
      acc.flatMap { list =>
        (t.kind, t.refundOf) match {
          case (TxKind.Income, Some(orig)) =>
            conv(-t.amountMinor).map(a => Entry(t.id, t.date, t.walletId, CategoryKind.Expense,
              t.categoryId.orElse(byId.get(orig).flatMap(_.categoryId)), t.payeeId, a, t.excludedBudgets) :: list)
          case (kind, _) =>
            val ck = if (kind == TxKind.Income) CategoryKind.Income else CategoryKind.Expense
            val parts = if (t.splits.nonEmpty) t.splits.map(s => (Option(s.categoryId), s.amountMinor)) else List((t.categoryId, t.amountMinor))
            parts.foldLeft[Either[BudgetError, List[Entry]]](Right(list)) { case (l, (cat, amt)) =>
              l.flatMap(xs => conv(amt).map(a => Entry(t.id, t.date, t.walletId, ck, cat, t.payeeId, a, t.excludedBudgets) :: xs))
            }
        }
      }
    }.map(_.reverse)
  }

  private def within(es: List[Entry], p: Period) = es.filter(e => p.contains(e.date))

  def summary(es: List[Entry], p: Period, currency: CurrencyCode): Summary = {
    val in      = within(es, p)
    val income  = in.filter(_.kind == CategoryKind.Income).map(_.amountMinor).sum
    val expense = in.filter(_.kind == CategoryKind.Expense).map(_.amountMinor).sum
    Summary(p.from, p.to, currency, income, expense, income - expense,
            Option.when(income > 0)(pct(BigDecimal(income - expense), BigDecimal(income))),
            minor(BigDecimal(expense) / p.days), in.map(_.txId).distinct.size)
  }

  /** Totals per top-level category (subcategories roll up and are listed beneath). */
  def byCategory(es: List[Entry], p: Period, kind: CategoryKind, categories: List[Category]): List[CategoryTotal] = {
    val cats  = categories.map(c => c.id -> c).toMap
    val in    = within(es, p).filter(_.kind == kind)
    val total = BigDecimal(in.map(_.amountMinor).sum)
    def name(id: Option[CategoryId]) = id.flatMap(cats.get).fold("Uncategorized")(c => c.name.value: String)
    def top(id: Option[CategoryId])  = id.flatMap(cats.get).fold(id)(c => c.parentId.orElse(Some(c.id)))
    in.groupBy(e => top(e.categoryId)).toList.map { case (parent, rows) =>
      val subs = rows.filter(r => r.categoryId != parent).groupBy(_.categoryId).toList.map { case (sub, rs) =>
        val a = rs.map(_.amountMinor).sum
        CategoryTotal(sub, name(sub), a, pct(BigDecimal(a), total), Nil)
      }.sortBy(-_.amountMinor)
      val a = rows.map(_.amountMinor).sum
      CategoryTotal(parent, name(parent), a, pct(BigDecimal(a), total), subs)
    }.sortBy(-_.amountMinor)
  }

  def bucketStart(d: LocalDate, b: Bucket): LocalDate = b match {
    case Bucket.Day   => d
    case Bucket.Week  => d.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))
    case Bucket.Month => d.withDayOfMonth(1)
  }

  /** Income and spending per bucket, including empty buckets. */
  def trend(es: List[Entry], p: Period, b: Bucket): List[TrendPoint] = {
    val step = b match { case Bucket.Day => ChronoUnit.DAYS; case Bucket.Week => ChronoUnit.WEEKS; case Bucket.Month => ChronoUnit.MONTHS }
    val starts = Iterator.iterate(bucketStart(p.from, b))(_.plus(1, step)).takeWhile(!_.isAfter(p.to)).toList
    val grouped = within(es, p).groupBy(e => bucketStart(e.date, b))
    starts.map { s =>
      val rows = grouped.getOrElse(s, Nil)
      val i = rows.filter(_.kind == CategoryKind.Income).map(_.amountMinor).sum
      val x = rows.filter(_.kind == CategoryKind.Expense).map(_.amountMinor).sum
      TrendPoint(s, i, x, i - x)
    }
  }

  /** The same-length period immediately before `p`. */
  def previousPeriod(p: Period): Period = Period(p.from.minusDays(p.days), p.from.minusDays(1))

  /** Spending per top-level category against the previous period, biggest increase first. */
  def compare(es: List[Entry], p: Period, categories: List[Category]): List[CategoryChange] = {
    val now  = byCategory(es, p, CategoryKind.Expense, categories).map(c => c.categoryId -> c).toMap
    val before = byCategory(es, previousPeriod(p), CategoryKind.Expense, categories).map(c => c.categoryId -> c).toMap
    (now.keySet ++ before.keySet).toList.map { id =>
      val cur  = now.get(id).fold(0L)(_.amountMinor)
      val prev = before.get(id).fold(0L)(_.amountMinor)
      CategoryChange(id, now.get(id).orElse(before.get(id)).fold("Uncategorized")(_.name), cur, prev, cur - prev,
                     Option.when(prev != 0)(pct(BigDecimal(cur - prev), BigDecimal(prev))))
    }.sortBy(c => -c.changeMinor)
  }

  /** Money flow for a Sankey chart: income categories into "Income", out to spending categories and savings. */
  def cashflow(es: List[Entry], p: Period, categories: List[Category]): List[FlowLink] = {
    val income  = byCategory(es, p, CategoryKind.Income, categories)
    val expense = byCategory(es, p, CategoryKind.Expense, categories)
    val in      = income.map(_.amountMinor).sum
    val out     = expense.map(_.amountMinor).sum
    income.filter(_.amountMinor > 0).map(c => FlowLink(c.name, "Income", c.amountMinor)) ++
      Option.when(out > in)(FlowLink("Deficit", "Income", out - in)).toList ++
      expense.filter(_.amountMinor > 0).map(c => FlowLink("Income", c.name, c.amountMinor)) ++
      Option.when(in > out)(FlowLink("Income", "Savings", in - out)).toList
  }

  /** Signed effect of a transaction on each wallet it touches, in the wallet's own currency. */
  def effects(t: Transaction): List[(WalletId, Long)] = t.kind match {
    case TxKind.Income   => List(t.walletId -> t.amountMinor)
    case TxKind.Expense  => List(t.walletId -> -t.amountMinor)
    case TxKind.Transfer => (t.walletId -> -t.amountMinor) :: t.toWalletId.map(_ -> t.toAmountMinor.getOrElse(t.amountMinor)).toList
  }

  def balances(wallets: List[Wallet], txs: List[Transaction], asOf: LocalDate, fx: FxTable, currency: CurrencyCode): Either[BudgetError, List[WalletBalance]] = {
    val moved = txs.filter(!_.date.isAfter(asOf)).flatMap(effects).groupMapReduce(_._1)(_._2)(_ + _)
    wallets.foldLeft[Either[BudgetError, List[WalletBalance]]](Right(Nil)) { (acc, w) =>
      val native = w.openingBalanceMinor + moved.getOrElse(w.id, 0L)
      acc.flatMap(l => convert(fx, native, w.currency, currency, asOf).map(b => WalletBalance(w.id, w.name, w.kind, w.currency, native, b) :: l))
    }.map(_.reverse)
  }

  /** Consecutive days with at least one transaction, counting back from today (or yesterday if nothing yet today). */
  def streak(dates: Set[LocalDate], today: LocalDate): Streak = {
    val start   = if (dates(today)) today else today.minusDays(1)
    val current = Iterator.iterate(start)(_.minusDays(1)).takeWhile(dates).size
    val longest = dates.toList.sorted.foldLeft((0, 0, Option.empty[LocalDate])) { case ((best, run, prev), d) =>
      val r = if (prev.contains(d.minusDays(1))) run + 1 else 1
      (best.max(r), r, Some(d))
    }._1
    Streak(current, longest, dates(today))
  }

  def recap(es: List[Entry], month: YearMonth, categories: List[Category], currency: CurrencyCode): Recap = {
    val p    = Period(month.atDay(1), month.atEndOfMonth)
    val prev = Period(month.minusMonths(1).atDay(1), month.minusMonths(1).atEndOfMonth)
    val s    = summary(es, p, currency)
    val prevExpense = summary(es, prev, currency).expenseMinor
    val largest = within(es, p).filter(_.kind == CategoryKind.Expense).groupMapReduce(_.txId)(_.amountMinor)(_ + _).maxByOption(_._2).map(_._1)
    Recap(month, s, byCategory(es, p, CategoryKind.Expense, categories).take(3), largest, prevExpense,
          Option.when(prevExpense != 0)(pct(BigDecimal(s.expenseMinor - prevExpense), BigDecimal(prevExpense))))
  }

  // ---- budgets ----

  /** A category filter covers its subcategories too. */
  private def expand(ids: List[CategoryId], categories: List[Category]): Set[CategoryId] =
    ids.toSet ++ categories.filter(c => c.parentId.exists(ids.contains)).map(_.id)

  /** Entries a budget counts in `p` (already in the budget's currency). */
  def counted(b: Budget, es: List[Entry], p: Period, categories: List[Category]): List[Entry] = {
    val kind     = if (b.income) CategoryKind.Income else CategoryKind.Expense
    val included = expand(b.categoryIds, categories)
    val excluded = expand(b.excludedCategoryIds, categories)
    within(es, p).filter { e =>
      e.kind == kind &&
      !e.excludedBudgets.contains(b.id) &&
      (b.walletIds.isEmpty || b.walletIds.contains(e.walletId)) &&
      (b.categoryIds.isEmpty || e.categoryId.exists(included.contains)) &&
      !e.categoryId.exists(excluded.contains)
    }
  }

  def status(b: Budget, es: List[Entry], categories: List[Category], asOf: LocalDate): Option[BudgetStatus] =
    Periods.periodOf(b, asOf).map { p =>
      val spent    = counted(b, es, p, categories).map(_.amountMinor).sum
      val rollover = if (!b.rollover) 0L else Periods.previous(b, p).fold(0L)(prev =>
                       (b.amountMinor - counted(b, es, prev, categories).map(_.amountMinor).sum).max(0L))
      val available = b.amountMinor + rollover
      val remaining = available - spent
      val elapsed   = ChronoUnit.DAYS.between(p.from, asOf) + 1
      val daysLeft  = p.days - elapsed + 1
      val cats      = categories.map(c => c.id -> c).toMap
      val spentBy   = counted(b, es, p, categories).flatMap(e => e.categoryId.map(id => cats.get(id).flatMap(_.parentId).getOrElse(id) -> e.amountMinor))
                        .groupMapReduce(_._1)(_._2)(_ + _)
      val limits    = b.categoryLimits.map(l => l.categoryId -> l.amountMinor).toMap
      val catRows   = (spentBy.keySet ++ limits.keySet).toList.map { id =>
                        LimitStatus(id, cats.get(id).fold("Uncategorized")(c => c.name.value: String), limits.get(id), spentBy.getOrElse(id, 0L))
                      }.sortBy(-_.spentMinor)
      BudgetStatus(b.id, b.name, p, b.currency, b.amountMinor, rollover, available, spent, remaining,
                   pct(BigDecimal(spent), BigDecimal(available)), daysLeft,
                   if (daysLeft > 0) (remaining.max(0L)) / daysLeft else 0L,
                   minor(BigDecimal(spent) / elapsed * p.days), spent > available, catRows)
    }

  /** Past periods (most recent first) with what was counted in each, for history charts. */
  def history(b: Budget, es: List[Entry], categories: List[Category], asOf: LocalDate, periods: Int): List[(Period, Long)] =
    Iterator.iterate(Periods.periodOf(b, asOf))(_.flatMap(Periods.previous(b, _))).takeWhile(_.isDefined).take(periods)
      .flatten.map(p => p -> counted(b, es, p, categories).map(_.amountMinor).sum).toList

  // ---- objectives ----

  /** `linked` are the objective's transactions with amounts already in its currency. */
  def objective(o: Objective, linked: List[(TxKind, Long)]): ObjectiveProgress = {
    val incoming = linked.collect { case (TxKind.Income, a) => a }.sum
    val outgoing = linked.collect { case (TxKind.Expense | TxKind.Transfer, a) => a }.sum
    val (principal, repaid, amount) = o.kind match {
      case ObjectiveKind.Goal     => (outgoing, incoming, outgoing - incoming)
      case ObjectiveKind.Lent     => (outgoing, incoming, outgoing - incoming)
      case ObjectiveKind.Borrowed => (incoming, outgoing, incoming - outgoing)
    }
    val (remaining, done) = o.kind match {
      case ObjectiveKind.Goal => ((o.targetMinor - amount).max(0L), pct(BigDecimal(amount), BigDecimal(o.targetMinor)))
      case _                  => (amount.max(0L), pct(BigDecimal(repaid), BigDecimal(principal)))
    }
    ObjectiveProgress(o, principal, repaid, amount, remaining, done.min(100), linked.size)
  }
}
