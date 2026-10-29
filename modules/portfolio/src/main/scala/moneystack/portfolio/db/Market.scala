package moneystack.portfolio.db

import moneystack.portfolio.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** The shared instrument catalogue with classifications, quotes and FX rates. */
trait Market[F[_]] {
  def createAsset(a: Asset): F[Asset]
  def assets(): F[List[Asset]]
  def assetBySymbol(s: Symbol): F[Option[Asset]]

  /** Replaces an asset's categories under one taxonomy. */
  def classify(asset: AssetId, taxonomy: Taxonomy, cs: List[Classification]): F[Unit]
  def classifications(): F[List[Classification]]

  def upsertQuotes(qs: List[Quote]): F[Unit]
  def quotes(assets: List[AssetId]): F[List[Quote]]
  def upsertFx(rs: List[FxRate]): F[Unit]
  def fxRates(): F[List[FxRate]]
}

object Market {

  import Codecs.{asset as assetC, assetId as assetIdC, classification as classC, fxRate as fxC, quote as quoteC,
                 symbol as symbolC, taxonomy as taxonomyC}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Market[F] = new Market[F] {

    def createAsset(a: Asset): F[Asset] = pool.use(_.prepare(Q.insertAsset).flatMap(_.unique(a)))

    def assets(): F[List[Asset]] = pool.use(_.prepare(Q.assets).flatMap(_.stream(skunk.Void, 256).compile.toList))

    def assetBySymbol(s: Symbol): F[Option[Asset]] = pool.use(_.prepare(Q.bySymbol).flatMap(_.option(s)))

    def classify(asset: AssetId, taxonomy: Taxonomy, cs: List[Classification]): F[Unit] =
      pool.use { s =>
        s.transaction.use { _ =>
          for {
            _ <- s.prepare(Q.clearClassification).flatMap(_.execute((asset, taxonomy)))
            _ <- s.prepare(Q.insertClassification).flatMap(pc => cs.traverse_(pc.execute))
          } yield ()
        }
      }

    def classifications(): F[List[Classification]] =
      pool.use(_.prepare(Q.classifications).flatMap(_.stream(skunk.Void, 256).compile.toList))

    def upsertQuotes(qs: List[Quote]): F[Unit] =
      pool.use(s => s.transaction.use(_ => s.prepare(Q.upsertQuote).flatMap(pc => qs.traverse_(pc.execute))))

    def quotes(assets: List[AssetId]): F[List[Quote]] =
      if (assets.isEmpty) Concurrent[F].pure(Nil)
      else pool.use(_.prepare(Q.quotes(assets.size)).flatMap(_.stream(assets, 1024).compile.toList))

    def upsertFx(rs: List[FxRate]): F[Unit] =
      pool.use(s => s.transaction.use(_ => s.prepare(Q.upsertFx).flatMap(pc => rs.traverse_(pc.execute))))

    def fxRates(): F[List[FxRate]] = pool.use(_.prepare(Q.fxRates).flatMap(_.stream(skunk.Void, 1024).compile.toList))
  }

  private object Q {

    private val assetCols = sql"id, symbol, name, kind, currency, created_at"

    val insertAsset: Query[Asset, Asset] =
      sql"INSERT INTO portfolio.assets ($assetCols) VALUES ${assetC.values} RETURNING $assetCols".query(assetC)

    val assets: Query[skunk.Void, Asset] = sql"SELECT $assetCols FROM portfolio.assets ORDER BY symbol".query(assetC)

    val bySymbol: Query[Symbol, Asset] = sql"SELECT $assetCols FROM portfolio.assets WHERE symbol = $symbolC".query(assetC)

    val clearClassification: Command[(AssetId, Taxonomy)] =
      sql"DELETE FROM portfolio.classifications WHERE asset_id = $assetIdC AND taxonomy = $taxonomyC".command

    val insertClassification: Command[Classification] =
      sql"INSERT INTO portfolio.classifications (asset_id, taxonomy, category, weight_bps) VALUES ${classC.values}".command

    val classifications: Query[skunk.Void, Classification] =
      sql"SELECT asset_id, taxonomy, category, weight_bps FROM portfolio.classifications".query(classC)

    val upsertQuote: Command[Quote] =
      sql"""INSERT INTO portfolio.quotes (asset_id, quote_date, close) VALUES ${quoteC.values}
            ON CONFLICT (asset_id, quote_date) DO UPDATE SET close = EXCLUDED.close""".command

    def quotes(n: Int): Query[List[AssetId], Quote] =
      sql"SELECT asset_id, quote_date, close FROM portfolio.quotes WHERE asset_id IN (${assetIdC.list(n)})".query(quoteC)

    val upsertFx: Command[FxRate] =
      sql"""INSERT INTO portfolio.fx_rates (from_ccy, to_ccy, rate_date, rate) VALUES ${fxC.values}
            ON CONFLICT (from_ccy, to_ccy, rate_date) DO UPDATE SET rate = EXCLUDED.rate""".command

    val fxRates: Query[skunk.Void, FxRate] = sql"SELECT from_ccy, to_ccy, rate_date, rate FROM portfolio.fx_rates".query(fxC)
  }
}
