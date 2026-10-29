package wealth.external

import wealth.domain.*

import cats.effect.*

import java.time.Instant

/**
 * Market-data provider abstraction.
 *
 * One trait fronts every upstream feed (AlphaVantage, Yahoo Finance, Investing.com,
 * S&P Global). The Iron constraints on [[Quote]] mean upstream payloads are
 * rejected at the boundary if the price isn't > 0 or the ticker doesn't match
 * our regex — so the rest of the stack can rely on the values.
 */
trait MarketData[F[_]] {
  def quote(ticker: Ticker, exchange: Exchange): F[Option[Quote]]
  def quotes(refs: List[(Ticker, Exchange)]): F[List[Quote]]
}

/** Composes multiple providers: try them in order; first successful response wins. */
final class CompositeMarketData[F[_]: Concurrent](providers: List[MarketData[F]]) extends MarketData[F] {
  import cats.syntax.all.*

  def quote(t: Ticker, e: Exchange): F[Option[Quote]] =
    providers.foldLeftM[F, Option[Quote]](None) {
      case (Some(q), _) => Concurrent[F].pure(Some(q))
      case (None,    p) => p.quote(t, e)
    }

  def quotes(refs: List[(Ticker, Exchange)]): F[List[Quote]] =
    refs.traverse { case (t, e) => quote(t, e) }.map(_.flatten)
}

/** In-memory sandbox useful for local dev / tests. */
object SandboxMarketData {
  def make[F[_]: Sync](snapshot: Map[(Ticker, Exchange), Quote]): F[MarketData[F]] =
    Sync[F].pure(new MarketData[F] {
      def quote(t: Ticker, e: Exchange): F[Option[Quote]] = Sync[F].pure(snapshot.get((t, e)))
      def quotes(refs: List[(Ticker, Exchange)]): F[List[Quote]] =
        Sync[F].pure(refs.flatMap(r => snapshot.get(r)))
    })
}

object MarketData {
  /** Convenience constructor for a single dummy quote, used in fallback paths. */
  def dummyQuote(t: Ticker, e: Exchange, c: CurrencyCode, price: BigDecimal): Quote =
    Quote(
      ticker   = t,
      exchange = e,
      price    = UnitPrice.positive(price).fold(sys.error, identity),
      currency = c,
      asOf     = Instant.now(),
      source   = "sandbox"
    )
}
