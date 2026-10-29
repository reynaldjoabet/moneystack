package moneystack.service

import moneystack.domain.*
import moneystack.db.{Holdings, Portfolios}
import moneystack.external.MarketData

import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate}

/**
 * Portfolio-level operations: build the live valuation, derive performance
 * metrics, validate allocations sum to 100%.
 */
trait PortfolioService[F[_]] {

  def value(p: Portfolio): F[PortfolioService.Valuation]
  def performance(p: Portfolio, asOf: LocalDate): F[PortfolioPerformance]
  def saveModel(mp: ModelPortfolio, alloc: List[ModelAllocation]): F[Either[PortfolioService.Error, ModelPortfolio]]
}

object PortfolioService {

  sealed trait Error
  object Error {
    final case class AllocationsNotFullyWeighted(totalBps: Int) extends Error
    final case class DuplicateInstrument(ticker: Ticker, exchange: Exchange) extends Error
  }

  final case class Valuation(
      portfolioId: PortfolioId,
      asOf: Instant,
      lines: List[ValuationLine],
      totalMarketValueMinor: AmountMinor,
      totalCostBasisMinor: AmountMinor,
      unrealizedPnLMinor: AmountMinor
  )

  final case class ValuationLine(
      ticker: Ticker,
      exchange: Exchange,
      quantity: Quantity,
      avgCostUnit: UnitPrice,
      marketPrice: Option[UnitPrice],
      marketValueMinor: AmountMinor,
      costBasisMinor: AmountMinor,
      unrealizedPnLMinor: AmountMinor
  )

  def make[F[_]: Async](
      portfolios: Portfolios[F],
      holdings: Holdings[F],
      marketData: MarketData[F]
  ): PortfolioService[F] = new PortfolioService[F] {

    def value(p: Portfolio): F[Valuation] =
      for {
        hs    <- holdings.listFor(p.id)
        qts   <- marketData.quotes(hs.map(h => (h.ticker, h.exchange)))
        index  = qts.map(q => (q.ticker, q.exchange) -> q).toMap
        lines  = hs.map { h =>
                   val quote = index.get((h.ticker, h.exchange))
                   val mvBd  = quote.fold(BigDecimal(0))(_.price.value * h.quantity.value)
                   val cbBd  = h.avgCostUnit.value * h.quantity.value
                   ValuationLine(
                     ticker             = h.ticker,
                     exchange           = h.exchange,
                     quantity           = h.quantity,
                     avgCostUnit        = h.avgCostUnit,
                     marketPrice        = quote.map(_.price),
                     marketValueMinor   = AmountMinor.assume((mvBd * 100).toLong),
                     costBasisMinor     = AmountMinor.assume((cbBd * 100).toLong),
                     unrealizedPnLMinor = AmountMinor.assume(((mvBd - cbBd) * 100).toLong)
                   )
                 }
        now <- Async[F].delay(Instant.now())
      } yield Valuation(
        portfolioId           = p.id,
        asOf                  = now,
        lines                 = lines,
        totalMarketValueMinor = AmountMinor.assume(lines.map(_.marketValueMinor.value).sum),
        totalCostBasisMinor   = AmountMinor.assume(lines.map(_.costBasisMinor.value).sum),
        unrealizedPnLMinor    = AmountMinor.assume(lines.map(_.unrealizedPnLMinor.value).sum)
      )

    def performance(p: Portfolio, asOf: LocalDate): F[PortfolioPerformance] =
      value(p).map { v =>
        // Simple TWR proxy: realized + unrealized over cost basis. A production
        // implementation needs cashflow-aware daily NAV and a Modified Dietz
        // / true TWR calculation against a benchmark.
        val cost  = v.totalCostBasisMinor.value.toDouble
        val pnl   = v.unrealizedPnLMinor.value.toDouble
        val twr   = if (cost == 0) 0 else math.round(pnl / cost * 10_000).toInt
        val twrCapped = twr.max(0).min(100_000)
        PortfolioPerformance(
          portfolioId        = p.id,
          asOf               = asOf,
          marketValueMinor   = v.totalMarketValueMinor,
          costBasisMinor     = v.totalCostBasisMinor,
          unrealizedPnLMinor = v.unrealizedPnLMinor,
          twrBpsToDate       = Bps.assume(twrCapped)
        )
      }

    def saveModel(mp: ModelPortfolio, alloc: List[ModelAllocation]): F[Either[Error, ModelPortfolio]] = {
      val total = alloc.map(_.weightBps.value).sum
      val dups  = alloc.groupBy(a => (a.ticker, a.exchange)).collectFirst {
        case ((t, e), rows) if rows.size > 1 => (t, e)
      }
      if (dups.isDefined)
        Async[F].pure(Left(Error.DuplicateInstrument(dups.get._1, dups.get._2)))
      else if (total != 10_000)
        Async[F].pure(Left(Error.AllocationsNotFullyWeighted(total)))
      else
        portfolios.createModel(mp, alloc).map(Right(_))
    }
  }
}
