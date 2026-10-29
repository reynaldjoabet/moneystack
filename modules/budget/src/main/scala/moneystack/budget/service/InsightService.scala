package moneystack.budget.service

import moneystack.domain.*
import moneystack.budget.domain.*

import cats.effect.*
import cats.syntax.all.*

import java.time.{LocalDate, YearMonth}

/**
 * Analysis over the wallets a user can see: totals, category breakdowns,
 * trends, period comparison, cash flow, balances and their history,
 * logging streaks and monthly recaps. Amounts are in the requested currency.
 */
trait InsightService[F[_]] {
  def summary(user: UserId, period: Period, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, Summary]]
  def categories(user: UserId, period: Period, kind: CategoryKind, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, List[CategoryTotal]]]
  def trend(user: UserId, period: Period, bucket: Bucket, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, List[TrendPoint]]]
  def compare(user: UserId, period: Period, currency: CurrencyCode): F[Either[BudgetError, List[CategoryChange]]]
  def cashflow(user: UserId, period: Period, currency: CurrencyCode): F[Either[BudgetError, List[FlowLink]]]
  def balances(user: UserId, asOf: LocalDate, currency: CurrencyCode): F[Either[BudgetError, InsightService.Balances]]
  /** Total balance at each month end in the period, and at its end. */
  def balanceHistory(user: UserId, period: Period, currency: CurrencyCode): F[Either[BudgetError, List[(LocalDate, Long)]]]
  def streak(user: UserId, today: LocalDate): F[Streak]
  def recap(user: UserId, month: YearMonth, currency: CurrencyCode): F[Either[BudgetError, Recap]]
}

object InsightService {

  /** `netWorthMinor` sums every wallet (credit cards and loans count as they stand, usually negative). */
  final case class Balances(asOf: LocalDate, currency: CurrencyCode, wallets: List[WalletBalance], netWorthMinor: Long)

  def make[F[_]: Async](loader: Loader[F]): InsightService[F] = new InsightService[F] {

    private def entries(user: UserId, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, (Snapshot, List[Entry])]] =
      loader.load(user).map(snap => snap.entries(currency, wallets).map(snap -> _))

    def summary(user: UserId, period: Period, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, Summary]] =
      entries(user, currency, wallets).map(_.map { case (_, es) => Reports.summary(es, period, currency) })

    def categories(user: UserId, period: Period, kind: CategoryKind, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, List[CategoryTotal]]] =
      entries(user, currency, wallets).map(_.map { case (snap, es) => Reports.byCategory(es, period, kind, snap.categories) })

    def trend(user: UserId, period: Period, bucket: Bucket, currency: CurrencyCode, wallets: Option[List[WalletId]]): F[Either[BudgetError, List[TrendPoint]]] =
      entries(user, currency, wallets).map(_.map { case (_, es) => Reports.trend(es, period, bucket) })

    def compare(user: UserId, period: Period, currency: CurrencyCode): F[Either[BudgetError, List[CategoryChange]]] =
      entries(user, currency, None).map(_.map { case (snap, es) => Reports.compare(es, period, snap.categories) })

    def cashflow(user: UserId, period: Period, currency: CurrencyCode): F[Either[BudgetError, List[FlowLink]]] =
      entries(user, currency, None).map(_.map { case (snap, es) => Reports.cashflow(es, period, snap.categories) })

    def balances(user: UserId, asOf: LocalDate, currency: CurrencyCode): F[Either[BudgetError, Balances]] =
      loader.load(user).map { snap =>
        Reports.balances(snap.wallets, snap.transactions, asOf, snap.fx, currency).map(bs => Balances(asOf, currency, bs, bs.map(_.baseMinor).sum))
      }

    def balanceHistory(user: UserId, period: Period, currency: CurrencyCode): F[Either[BudgetError, List[(LocalDate, Long)]]] =
      loader.load(user).map { snap =>
        val dates = (Iterator.iterate(YearMonth.from(period.from))(_.plusMonths(1)).takeWhile(!_.isAfter(YearMonth.from(period.to)))
                      .map(_.atEndOfMonth).filter(!_.isAfter(period.to)).toList :+ period.to).distinct
        dates.traverse(d => Reports.balances(snap.wallets, snap.transactions, d, snap.fx, currency).map(bs => d -> bs.map(_.baseMinor).sum))
      }

    def streak(user: UserId, today: LocalDate): F[Streak] =
      loader.load(user).map(snap => Reports.streak(snap.transactions.filter(_.createdBy == user).map(_.date).toSet, today))

    def recap(user: UserId, month: YearMonth, currency: CurrencyCode): F[Either[BudgetError, Recap]] =
      entries(user, currency, None).map(_.map { case (snap, es) => Reports.recap(es, month, snap.categories, currency) })
  }
}
