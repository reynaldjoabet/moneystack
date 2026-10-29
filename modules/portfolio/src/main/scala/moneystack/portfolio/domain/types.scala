package moneystack.portfolio.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import java.util.UUID

// ---------- Identifiers ----------

type InvestmentAccountId = InvestmentAccountId.T
object InvestmentAccountId extends RefinedType[UUID, Pure] {}

type AssetId             = AssetId.T
object AssetId             extends RefinedType[UUID, Pure] {}

type ActivityId          = ActivityId.T
object ActivityId          extends RefinedType[UUID, Pure] {}

type TargetId            = TargetId.T
object TargetId            extends RefinedType[UUID, Pure] {}

type LimitId             = LimitId.T
object LimitId             extends RefinedType[UUID, Pure] {}

type GoalId              = GoalId.T
object GoalId              extends RefinedType[UUID, Pure] {}

type AltAssetId          = AltAssetId.T
object AltAssetId          extends RefinedType[UUID, Pure] {}

type LiabilityId         = LiabilityId.T
object LiabilityId         extends RefinedType[UUID, Pure] {}

// ---------- Scalars ----------

/** Ticker or provider symbol: `AAPL`, `VWCE.DE`, `BTC-USD`, `^GSPC`. */
type SymbolConstraint = Match["^[A-Za-z0-9.\\-:^=_/]{1,32}$"]
type Symbol = Symbol.T
object Symbol extends RefinedType[String, SymbolConstraint] {}

/** Taxonomy key: `asset_class`, `sector`, `region`, or a custom one. */
type TaxonomyConstraint = Match["^[a-z][a-z0-9_]{0,31}$"]
type Taxonomy = Taxonomy.T
object Taxonomy extends RefinedType[String, TaxonomyConstraint] {
  val AssetClass: Taxonomy = Taxonomy.applyUnsafe("asset_class")
  val Sector: Taxonomy     = Taxonomy.applyUnsafe("sector")
  val Region: Taxonomy     = Taxonomy.applyUnsafe("region")
}
