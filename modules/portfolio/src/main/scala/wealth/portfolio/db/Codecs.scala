package wealth.portfolio.db

import wealth.domain.*
import wealth.portfolio.domain.*
import wealth.platform.CoreCodecs

import io.github.iltotore.iron.*
import io.github.iltotore.iron.skunk.*

import _root_.skunk.*
import _root_.skunk.codec.all.*

object Codecs {

  export CoreCodecs.{body, bps, currency, fine, instant, title, userId}

  /** Money in a NUMERIC(20, 6) column. */
  val money: Codec[BigDecimal]    = numeric(20, 6)
  /** Units held, NUMERIC(30, 10) for crypto precision. */
  val units: Codec[BigDecimal]    = numeric(30, 10)

  val accountId: Codec[InvestmentAccountId] = uuid.imap(InvestmentAccountId.assume)(_.value)
  val assetId: Codec[AssetId]               = uuid.imap(AssetId.assume)(_.value)
  val activityId: Codec[ActivityId]         = uuid.imap(ActivityId.assume)(_.value)
  val targetId: Codec[TargetId]             = uuid.imap(TargetId.assume)(_.value)
  val limitId: Codec[LimitId]               = uuid.imap(LimitId.assume)(_.value)
  val goalId: Codec[GoalId]                 = uuid.imap(GoalId.assume)(_.value)
  val altAssetId: Codec[AltAssetId]         = uuid.imap(AltAssetId.assume)(_.value)
  val liabilityId: Codec[LiabilityId]       = uuid.imap(LiabilityId.assume)(_.value)

  val symbol: Codec[Symbol]     = varchar(32).refined[SymbolConstraint].imap(Symbol.assume)(_.value)
  val taxonomy: Codec[Taxonomy] = varchar(32).refined[TaxonomyConstraint].imap(Taxonomy.assume)(_.value)

  val accountKind: Codec[AccountKind]     = CoreCodecs.enumOf(16, AccountKind.parse, AccountKind.render)
  val tracking: Codec[Tracking]           = CoreCodecs.enumOf(12, Tracking.parse, Tracking.render)
  val assetKind: Codec[AssetKind]         = CoreCodecs.enumOf(16, AssetKind.parse, AssetKind.render)
  val activityType: Codec[ActivityType]   = CoreCodecs.enumOf(16, ActivityType.parse, ActivityType.render)
  val altAssetKind: Codec[AltAssetKind]   = CoreCodecs.enumOf(16, AltAssetKind.parse, AltAssetKind.render)
  val vehicleClass: Codec[VehicleClass]   = CoreCodecs.enumOf(16, VehicleClass.parse, VehicleClass.render)
  val pace: Codec[DepreciationPace]       = CoreCodecs.enumOf(8, DepreciationPace.parse, DepreciationPace.render)
  val liabilityKind: Codec[LiabilityKind] = CoreCodecs.enumOf(16, LiabilityKind.parse, LiabilityKind.render)

  val account: Codec[InvestmentAccount] =
    (accountId *: userId *: title *: accountKind *: tracking *: currency *: title.opt *: bool *: instant).to[InvestmentAccount]

  val snapshot: Codec[BalanceSnapshot] =
    (accountId *: date *: money *: money).to[BalanceSnapshot]

  val asset: Codec[Asset] =
    (assetId *: symbol *: title *: assetKind *: currency *: instant).to[Asset]

  val classification: Codec[Classification] =
    (assetId *: taxonomy *: title *: bps).to[Classification]

  val quote: Codec[Quote] = (assetId *: date *: fine).to[Quote]

  val fxRate: Codec[FxRate] = (currency *: currency *: date *: fine).to[FxRate]

  val activity: Codec[Activity] =
    (activityId *: accountId *: assetId.opt *: activityType *: date *: units.opt *: fine.opt *: money.opt *: money *:
      currency *: bool *: accountId.opt *: body.opt *: varchar(64).opt *: instant).to[Activity]

  val target: Codec[AllocationTarget] =
    (targetId *: userId *: taxonomy *: title *: bps).to[AllocationTarget]

  /** A limit without its accounts, which live in `contribution_limit_accounts`. */
  val limit: Codec[ContributionLimit] =
    (limitId *: userId *: title *: int4 *: money *: currency *: date.opt *: date.opt).imap {
      case (id, owner, name, year, amount, ccy, from, to) => ContributionLimit(id, owner, name, year, amount, ccy, Nil, from, to)
    }(l => (l.id, l.ownerId, l.name, l.year, l.limitAmount, l.currency, l.startDate, l.endDate))

  /** A goal without its funding, which lives in `goal_funding`. */
  val goal: Codec[Goal] =
    (goalId *: userId *: title *: money *: currency *: date.opt *: money *: fine *: instant).imap {
      case (id, owner, name, target, ccy, by, monthly, ret, at) => Goal(id, owner, name, target, ccy, by, monthly, ret, Nil, at)
    }(g => (g.id, g.ownerId, g.name, g.targetAmount, g.currency, g.targetDate, g.monthlyContribution, g.expectedAnnualReturn, g.createdAt))

  val altAsset: Codec[AltAsset] =
    (altAssetId *: userId *: altAssetKind *: title *: currency *: money *: date *: bps *: money *: vehicleClass.opt *:
      pace.opt *: body.opt *: bool *: instant).to[AltAsset]

  val altValuation: Codec[AltValuation] = (altAssetId *: date *: money *: title).to[AltValuation]

  val liability: Codec[Liability] =
    (liabilityId *: userId *: liabilityKind *: title *: currency *: money *: fine *: int4 *: date *: money *:
      altAssetId.opt *: bool *: instant).to[Liability]

  val statement: Codec[LiabilityStatement] = (liabilityId *: date *: money).to[LiabilityStatement]
}
