package moneystack.portfolio.domain

import moneystack.domain.*

import java.time.LocalDate
import scala.collection.immutable.TreeMap

/** Value of a scope at the end of `date`, in base currency. `flow` is that day's external flow. */
final case class DailyValue(
    date: LocalDate,
    investments: BigDecimal,
    cash: BigDecimal,
    total: BigDecimal,
    costBasis: BigDecimal,
    netContribution: BigDecimal,
    flow: BigDecimal
)

/** A position as of a date. Market values fall back to the last trade price, then to cost (`priced = false`). */
final case class HoldingView(
    assetId: AssetId,
    symbol: Symbol,
    name: Title,
    kind: AssetKind,
    currency: CurrencyCode,
    quantity: BigDecimal,
    averageCost: BigDecimal,
    price: BigDecimal,
    priced: Boolean,
    marketValue: BigDecimal,
    marketValueBase: BigDecimal,
    costBase: BigDecimal,
    unrealizedBase: BigDecimal,
    unrealizedPct: Option[BigDecimal],
    weightPct: BigDecimal,
    accounts: List[InvestmentAccountId]
)

/** What a valuation covers: accounts plus their activities or balance snapshots. */
final case class Scope(
    accounts: List[InvestmentAccount],
    activities: Map[InvestmentAccountId, List[Activity]],
    snapshots: Map[InvestmentAccountId, List[BalanceSnapshot]]
) {
  val ids: Set[InvestmentAccountId] = accounts.map(_.id).toSet

  /** A transfer is a flow unless its other side is inside the scope. */
  def counts(f: Flow): Boolean = !(f.transfer && f.counterAccount.exists(ids.contains))
}

object Valuation {

  private def round2(x: BigDecimal) = x.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)

  /** Market price of a position on `date`, and whether it came from a quote. */
  def priceOf(pos: Position, ctx: Engine.Context, date: LocalDate): (BigDecimal, Boolean) =
    ctx.prices.close(pos.assetId, date) match {
      case Some(p) => (p, true)
      case None    => (pos.lastTradePrice.orElse(pos.averageCost).getOrElse(BigDecimal(0)), false)
    }

  /** Daily values from `from` to `to`; history before `from` is replayed but not emitted. */
  def series(scope: Scope, ctx: Engine.Context, from: LocalDate, to: LocalDate): Either[PortfolioError, List[DailyValue]] = {
    val tracked   = scope.accounts.filter(_.tracking == Tracking.Activities)
    val balanced  = scope.accounts.filter(_.tracking == Tracking.Balances)
    val byDay     = tracked.map(a => a.id -> Engine.ordered(scope.activities.getOrElse(a.id, Nil)).groupBy(_.date)).toMap
    val snaps     = balanced.map(a => a.id -> TreeMap.from(scope.snapshots.getOrElse(a.id, Nil).map(s => s.date -> s))).toMap
    val firstDays = byDay.values.flatMap(_.keys) ++ snaps.values.flatMap(_.keys)

    if (firstDays.isEmpty || firstDays.min.isAfter(to)) Right(Nil)
    else {
      val start = Ordering[LocalDate].min(firstDays.min, from)
      val days  = Iterator.iterate(start)(_.plusDays(1)).takeWhile(!_.isAfter(to)).toList

      type State = (Map[InvestmentAccountId, Book], BigDecimal, List[DailyValue])
      val init: State = (tracked.map(a => a.id -> Book.empty(a.id)).toMap, BigDecimal(0), Nil)

      days.foldLeft[Either[PortfolioError, State]](Right(init)) { (acc, day) =>
        acc.flatMap { case (books, contributed, out) =>
          for {
            stepped   <- tracked.foldLeft[Either[PortfolioError, Map[InvestmentAccountId, Book]]](Right(books)) { (bs, acct) =>
                           bs.flatMap { m =>
                             byDay(acct.id).getOrElse(day, Nil)
                               .foldLeft[Either[PortfolioError, Book]](Right(m(acct.id)))((b, a) => b.flatMap(Engine.step(_, a, ctx)))
                               .map(m.updated(acct.id, _))
                           }
                         }
            open       = stepped.values.toList
            todayFlow  = open.flatMap(b => b.flows.drop(books(b.accountId).flows.size)).filter(scope.counts).map(_.amountBase).sum
            invested  <- positionsValue(open, ctx, day)
            cash      <- cashValue(open, ctx, day)
            balances  <- balancedValue(balanced, snaps, ctx, day)
            (balValue, balFlow) = balances
            flow       = todayFlow + balFlow
            total      = invested._1 + cash + balValue
            dv         = DailyValue(day, round2(invested._1 + balValue), round2(cash), round2(total), round2(invested._2),
                                    round2(contributed + flow), round2(flow))
          } yield (stepped, contributed + flow, if (day.isBefore(from)) out else dv :: out)
        }
      }.map(_._3.reverse)
    }
  }

  private def positionsValue(books: List[Book], ctx: Engine.Context, day: LocalDate): Either[PortfolioError, (BigDecimal, BigDecimal)] =
    books.flatMap(_.openPositions).foldLeft[Either[PortfolioError, (BigDecimal, BigDecimal)]](Right((BigDecimal(0), BigDecimal(0)))) { (acc, pos) =>
      for {
        sum <- acc
        mv  <- ctx.fx.convert(pos.quantity * priceOf(pos, ctx, day)._1, pos.currency, ctx.base, day)
      } yield (sum._1 + mv, sum._2 + pos.costBase)
    }

  private def cashValue(books: List[Book], ctx: Engine.Context, day: LocalDate): Either[PortfolioError, BigDecimal] =
    books.flatMap(_.cash.toList).foldLeft[Either[PortfolioError, BigDecimal]](Right(BigDecimal(0))) { case (acc, (ccy, amt)) =>
      acc.flatMap(sum => ctx.fx.convert(amt, ccy, ctx.base, day).map(sum + _))
    }

  /** Latest snapshot value of balance accounts, and the change in invested amount on snapshot days. */
  private def balancedValue(
      accounts: List[InvestmentAccount],
      snaps: Map[InvestmentAccountId, TreeMap[LocalDate, BalanceSnapshot]],
      ctx: Engine.Context,
      day: LocalDate
  ): Either[PortfolioError, (BigDecimal, BigDecimal)] =
    accounts.foldLeft[Either[PortfolioError, (BigDecimal, BigDecimal)]](Right((BigDecimal(0), BigDecimal(0)))) { (acc, a) =>
      val series = snaps(a.id)
      val latest = series.rangeTo(day).lastOption.map(_._2)
      val flow = series.get(day).map { s =>
        s.invested - series.rangeUntil(day).lastOption.map(_._2.invested).getOrElse(BigDecimal(0))
      }.getOrElse(BigDecimal(0))
      for {
        sum   <- acc
        value <- ctx.fx.convert(latest.map(_.balance).getOrElse(BigDecimal(0)), a.currency, ctx.base, day)
        f     <- ctx.fx.convert(flow, a.currency, ctx.base, day)
      } yield (sum._1 + value, sum._2 + f)
    }

  /** Open positions across `books`, merged per asset and valued on `asOf`. */
  def holdings(books: List[Book], ctx: Engine.Context, asOf: LocalDate): Either[PortfolioError, List[HoldingView]] = {
    val byAsset = books.flatMap(b => b.openPositions.map(b.accountId -> _)).groupBy(_._2.assetId)
    byAsset.toList.foldLeft[Either[PortfolioError, List[HoldingView]]](Right(Nil)) { case (acc, (assetId, held)) =>
      val asset  = ctx.assets(assetId)
      val merged = Position(assetId, asset.currency, held.flatMap(_._2.lots).toVector, held.flatMap(_._2.lastTradePrice).lastOption)
      val (price, priced) = priceOf(merged, ctx, asOf)
      val mv = merged.quantity * price
      for {
        list   <- acc
        mvBase <- ctx.fx.convert(mv, asset.currency, ctx.base, asOf)
      } yield HoldingView(assetId, asset.symbol, asset.name, asset.kind, asset.currency, merged.quantity,
                          merged.averageCost.getOrElse(BigDecimal(0)), price, priced, round2(mv), round2(mvBase),
                          round2(merged.costBase), round2(mvBase - merged.costBase),
                          Option.when(merged.costBase != 0)(((mvBase - merged.costBase) / merged.costBase * 100).setScale(4, BigDecimal.RoundingMode.HALF_EVEN)),
                          BigDecimal(0), held.map(_._1).distinct) :: list
    }.map { hs =>
      val total = hs.map(_.marketValueBase).sum
      hs.map(h => h.copy(weightPct = if (total == 0) BigDecimal(0) else (h.marketValueBase / total * 100).setScale(4, BigDecimal.RoundingMode.HALF_EVEN)))
        .sortBy(-_.marketValueBase)
    }
  }
}
