package moneystack.portfolio.db

import moneystack.domain.*
import moneystack.portfolio.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Property, vehicles and other holdings outside brokerage accounts, plus loans. Scoped to an owner. */
trait BalanceSheet[F[_]] {
  def createAsset(a: AltAsset): F[AltAsset]
  def assets(owner: UserId): F[List[AltAsset]]
  def setAssetActive(owner: UserId, id: AltAssetId, active: Boolean): F[Option[AltAsset]]
  def upsertValuation(v: AltValuation): F[Unit]
  def valuations(owner: UserId): F[List[AltValuation]]

  def createLiability(l: Liability): F[Liability]
  def liabilities(owner: UserId): F[List[Liability]]
  def setLiabilityActive(owner: UserId, id: LiabilityId, active: Boolean): F[Option[Liability]]
  def upsertStatement(s: LiabilityStatement): F[Unit]
  def statements(owner: UserId): F[List[LiabilityStatement]]
}

object BalanceSheet {

  import Codecs.{
    altAsset as altC, altAssetId as altIdC, altValuation as valuationC, liability as liabilityC,
    liabilityId as liabilityIdC, statement as statementC, userId as userIdC
  }
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): BalanceSheet[F] = new BalanceSheet[F] {

    def createAsset(a: AltAsset): F[AltAsset] = pool.use(_.prepare(Q.insertAsset).flatMap(_.unique(a)))

    def assets(owner: UserId): F[List[AltAsset]] = pool.use(_.prepare(Q.assets).flatMap(_.stream(owner, 64).compile.toList))

    def setAssetActive(owner: UserId, id: AltAssetId, active: Boolean): F[Option[AltAsset]] =
      pool.use(_.prepare(Q.setAssetActive).flatMap(_.option((active, id, owner))))

    def upsertValuation(v: AltValuation): F[Unit] = pool.use(_.prepare(Q.upsertValuation).flatMap(_.execute(v))).void

    def valuations(owner: UserId): F[List[AltValuation]] =
      pool.use(_.prepare(Q.valuations).flatMap(_.stream(owner, 256).compile.toList))

    def createLiability(l: Liability): F[Liability] = pool.use(_.prepare(Q.insertLiability).flatMap(_.unique(l)))

    def liabilities(owner: UserId): F[List[Liability]] =
      pool.use(_.prepare(Q.liabilities).flatMap(_.stream(owner, 64).compile.toList))

    def setLiabilityActive(owner: UserId, id: LiabilityId, active: Boolean): F[Option[Liability]] =
      pool.use(_.prepare(Q.setLiabilityActive).flatMap(_.option((active, id, owner))))

    def upsertStatement(s: LiabilityStatement): F[Unit] = pool.use(_.prepare(Q.upsertStatement).flatMap(_.execute(s))).void

    def statements(owner: UserId): F[List[LiabilityStatement]] =
      pool.use(_.prepare(Q.statements).flatMap(_.stream(owner, 256).compile.toList))
  }

  private object Q {

    private val assetCols =
      sql"""id, owner_id, kind, name, currency, purchase_price, purchase_date, ownership_bps, acquisition_costs,
            vehicle_class, pace, notes, active, created_at"""

    val insertAsset: Query[AltAsset, AltAsset] =
      sql"INSERT INTO portfolio.alt_assets ($assetCols) VALUES ${altC.values} RETURNING $assetCols".query(altC)

    val assets: Query[UserId, AltAsset] =
      sql"SELECT $assetCols FROM portfolio.alt_assets WHERE owner_id = $userIdC ORDER BY name".query(altC)

    val setAssetActive: Query[(Boolean, AltAssetId, UserId), AltAsset] =
      sql"UPDATE portfolio.alt_assets SET active = $bool WHERE id = $altIdC AND owner_id = $userIdC RETURNING $assetCols".query(altC)

    val upsertValuation: Command[AltValuation] =
      sql"""INSERT INTO portfolio.alt_valuations (asset_id, valuation_date, value, source) VALUES ${valuationC.values}
            ON CONFLICT (asset_id, valuation_date) DO UPDATE SET value = EXCLUDED.value, source = EXCLUDED.source""".command

    val valuations: Query[UserId, AltValuation] =
      sql"""SELECT v.asset_id, v.valuation_date, v.value, v.source FROM portfolio.alt_valuations v
            JOIN portfolio.alt_assets a ON a.id = v.asset_id WHERE a.owner_id = $userIdC ORDER BY v.valuation_date""".query(valuationC)

    private val liabilityCols =
      sql"""id, owner_id, kind, name, currency, principal, annual_rate, term_months, start_date, extra_monthly_payment,
            secured_by, active, created_at"""

    val insertLiability: Query[Liability, Liability] =
      sql"INSERT INTO portfolio.liabilities ($liabilityCols) VALUES ${liabilityC.values} RETURNING $liabilityCols".query(liabilityC)

    val liabilities: Query[UserId, Liability] =
      sql"SELECT $liabilityCols FROM portfolio.liabilities WHERE owner_id = $userIdC ORDER BY name".query(liabilityC)

    val setLiabilityActive: Query[(Boolean, LiabilityId, UserId), Liability] =
      sql"""UPDATE portfolio.liabilities SET active = $bool WHERE id = $liabilityIdC AND owner_id = $userIdC
            RETURNING $liabilityCols""".query(liabilityC)

    val upsertStatement: Command[LiabilityStatement] =
      sql"""INSERT INTO portfolio.liability_statements (liability_id, statement_date, balance) VALUES ${statementC.values}
            ON CONFLICT (liability_id, statement_date) DO UPDATE SET balance = EXCLUDED.balance""".command

    val statements: Query[UserId, LiabilityStatement] =
      sql"""SELECT s.liability_id, s.statement_date, s.balance FROM portfolio.liability_statements s
            JOIN portfolio.liabilities l ON l.id = s.liability_id WHERE l.owner_id = $userIdC ORDER BY s.statement_date""".query(statementC)
  }
}
