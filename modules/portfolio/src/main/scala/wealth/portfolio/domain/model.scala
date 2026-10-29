package wealth.portfolio.domain

import wealth.domain.*

import java.time.{Instant, LocalDate}

private[domain] trait Keyed[A] {
  def values: Array[A]
  def key(a: A): String
  def parse(s: String): Either[String, A] = values.find(key(_) == s.toLowerCase).toRight(s"unknown value: $s")
  def render(a: A): String               = key(a)
}

// ---------- Accounts ----------

enum AccountKind(val key: String) {
  case Brokerage     extends AccountKind("brokerage")
  case Retirement    extends AccountKind("retirement")
  case TaxAdvantaged extends AccountKind("tax_advantaged")   // PEA, ISA, TFSA, ...
  case Crypto        extends AccountKind("crypto")
  case Savings       extends AccountKind("savings")
  case Cash          extends AccountKind("cash")
  case Other         extends AccountKind("other")
}
object AccountKind extends Keyed[AccountKind] { def key(a: AccountKind) = a.key }

/**
 * How an account is valued: `Activities` replays its activities against
 * quotes; `Balances` reads the latest manual balance snapshot (savings
 * books and other accounts tracked by statement balance).
 */
enum Tracking(val key: String) {
  case Activities extends Tracking("activities")
  case Balances   extends Tracking("balances")
}
object Tracking extends Keyed[Tracking] { def key(a: Tracking) = a.key }

final case class InvestmentAccount(
    id: InvestmentAccountId,
    ownerId: UserId,
    name: Title,
    kind: AccountKind,
    tracking: Tracking,
    currency: CurrencyCode,
    group: Option[Title],
    active: Boolean,
    createdAt: Instant
)

/** A statement balance for a `Balances` account; `invested` is the cumulative amount paid in. */
final case class BalanceSnapshot(accountId: InvestmentAccountId, date: LocalDate, balance: BigDecimal, invested: BigDecimal)

// ---------- Assets & market data ----------

enum AssetKind(val key: String) {
  case Equity    extends AssetKind("equity")
  case Etf       extends AssetKind("etf")
  case Fund      extends AssetKind("fund")
  case Bond      extends AssetKind("bond")
  case Crypto    extends AssetKind("crypto")
  case Commodity extends AssetKind("commodity")
  case Cash      extends AssetKind("cash")
  case Other     extends AssetKind("other")
}
object AssetKind extends Keyed[AssetKind] { def key(a: AssetKind) = a.key }

final case class Asset(id: AssetId, symbol: Symbol, name: Title, kind: AssetKind, currency: CurrencyCode, createdAt: Instant)

/** A weighted category under a taxonomy: an ETF can be 60% `region: US`, 40% `region: Europe`. */
final case class Classification(assetId: AssetId, taxonomy: Taxonomy, category: Title, weightBps: Bps)

final case class Quote(assetId: AssetId, date: LocalDate, close: BigDecimal)

// ---------- Activities ----------

enum ActivityType(val key: String) {
  case Buy         extends ActivityType("buy")
  case Sell        extends ActivityType("sell")
  case Dividend    extends ActivityType("dividend")
  case Interest    extends ActivityType("interest")
  case Deposit     extends ActivityType("deposit")
  case Withdrawal  extends ActivityType("withdrawal")
  case TransferIn  extends ActivityType("transfer_in")
  case TransferOut extends ActivityType("transfer_out")
  case Fee         extends ActivityType("fee")
  case Tax         extends ActivityType("tax")
  case Split       extends ActivityType("split")
  case Credit      extends ActivityType("credit")
  case Adjustment  extends ActivityType("adjustment")

  /** Types that act on a position and so need an asset. */
  def needsAsset: Boolean = this match {
    case Buy | Sell | Split | Adjustment => true
    case _                               => false
  }
}
object ActivityType extends Keyed[ActivityType] { def key(a: ActivityType) = a.key }

/**
 * A portfolio event. Amounts are in `currency` (the asset's for trades).
 *
 *  - Buy/Sell: `quantity` at `unitPrice`, plus `fee`.
 *  - Dividend/Interest/Deposit/Withdrawal/Fee/Tax/Credit: `amount` of cash.
 *  - TransferIn/TransferOut: `quantity` at `unitPrice` cost basis for an
 *    asset, or `amount` of cash. Internal unless `external`, in which case
 *    it is a contribution; `counterAccountId` names the other side.
 *  - Split: `amount` is the ratio (2 for a 2-for-1 split).
 *  - Adjustment: signed `quantity` change at no cash cost.
 */
final case class Activity(
    id: ActivityId,
    accountId: InvestmentAccountId,
    assetId: Option[AssetId],
    kind: ActivityType,
    date: LocalDate,
    quantity: Option[BigDecimal],
    unitPrice: Option[BigDecimal],
    amount: Option[BigDecimal],
    fee: BigDecimal,
    currency: CurrencyCode,
    external: Boolean,
    counterAccountId: Option[InvestmentAccountId],
    note: Option[Body],
    importKey: Option[String],
    createdAt: Instant
)

// ---------- Planning ----------

/** Target weight of a category under a taxonomy (`asset_kind` and `currency` are built in). */
final case class AllocationTarget(id: TargetId, ownerId: UserId, taxonomy: Taxonomy, category: Title, weightBps: Bps)

/** A yearly contribution allowance shared by a group of accounts (ISA, IRA, PEA ceilings). */
final case class ContributionLimit(
    id: LimitId,
    ownerId: UserId,
    name: Title,
    year: Int,
    limitAmount: BigDecimal,
    currency: CurrencyCode,
    accountIds: List[InvestmentAccountId],
    startDate: Option[LocalDate],
    endDate: Option[LocalDate]
)

/** A savings goal funded by a share of each linked account's value. */
final case class Goal(
    id: GoalId,
    ownerId: UserId,
    name: Title,
    targetAmount: BigDecimal,
    currency: CurrencyCode,
    targetDate: Option[LocalDate],
    monthlyContribution: BigDecimal,
    expectedAnnualReturn: BigDecimal,
    funding: List[GoalFunding],
    createdAt: Instant
)

final case class GoalFunding(accountId: InvestmentAccountId, shareBps: Bps)

// ---------- Net worth: alternative assets & liabilities ----------

enum AltAssetKind(val key: String) {
  case RealEstate  extends AltAssetKind("real_estate")
  case Vehicle     extends AltAssetKind("vehicle")
  case Collectible extends AltAssetKind("collectible")
  case Business    extends AltAssetKind("business")
  case Other       extends AltAssetKind("other")
}
object AltAssetKind extends Keyed[AltAssetKind] { def key(a: AltAssetKind) = a.key }

enum VehicleClass(val key: String, val curve: List[Int]) {
  case Sedan      extends VehicleClass("sedan", List(20, 15, 12, 10, 8, 7, 6, 5, 5, 4))
  case Suv        extends VehicleClass("suv", List(18, 14, 11, 10, 8, 7, 6, 5, 5, 4))
  case Truck      extends VehicleClass("truck", List(15, 12, 10, 9, 8, 7, 6, 5, 5, 4))
  case Luxury     extends VehicleClass("luxury", List(25, 18, 14, 12, 10, 8, 7, 6, 5, 5))
  case Ev         extends VehicleClass("ev", List(22, 17, 13, 10, 8, 7, 6, 5, 5, 4))
  case Motorcycle extends VehicleClass("motorcycle", List(20, 15, 12, 10, 8, 7, 6, 5, 5, 4))
  case OtherClass extends VehicleClass("other", List(20, 15, 12, 10, 8, 7, 6, 5, 5, 4))
}
object VehicleClass extends Keyed[VehicleClass] { def key(a: VehicleClass) = a.key }

enum DepreciationPace(val key: String, val multiplier: BigDecimal) {
  case Slow    extends DepreciationPace("slow", BigDecimal("0.7"))
  case Average extends DepreciationPace("average", BigDecimal(1))
  case Fast    extends DepreciationPace("fast", BigDecimal("1.3"))
}
object DepreciationPace extends Keyed[DepreciationPace] { def key(a: DepreciationPace) = a.key }

/**
 * Property, vehicles, collectibles, private businesses. Valued from manual
 * valuations; vehicles without one depreciate from the purchase price.
 * `ownershipBps` is the owner's share (joint property).
 */
final case class AltAsset(
    id: AltAssetId,
    ownerId: UserId,
    kind: AltAssetKind,
    name: Title,
    currency: CurrencyCode,
    purchasePrice: BigDecimal,
    purchaseDate: LocalDate,
    ownershipBps: Bps,
    acquisitionCosts: BigDecimal,
    vehicleClass: Option[VehicleClass],
    pace: Option[DepreciationPace],
    notes: Option[Body],
    active: Boolean,
    createdAt: Instant
)

final case class AltValuation(assetId: AltAssetId, date: LocalDate, value: BigDecimal, source: Title)

enum LiabilityKind(val key: String) {
  case Mortgage   extends LiabilityKind("mortgage")
  case Auto       extends LiabilityKind("auto")
  case Student    extends LiabilityKind("student")
  case Personal   extends LiabilityKind("personal")
  case Heloc      extends LiabilityKind("heloc")
  case Business   extends LiabilityKind("business")
  case Medical    extends LiabilityKind("medical")
  case CreditCard extends LiabilityKind("credit_card")
  case Other      extends LiabilityKind("other")
}
object LiabilityKind extends Keyed[LiabilityKind] { def key(a: LiabilityKind) = a.key }

/**
 * An amortising loan: `principal` at `annualRate` over `termMonths` from
 * `startDate`, with optional extra monthly principal. Manual balance
 * statements, when present, override the schedule from their date on.
 */
final case class Liability(
    id: LiabilityId,
    ownerId: UserId,
    kind: LiabilityKind,
    name: Title,
    currency: CurrencyCode,
    principal: BigDecimal,
    annualRate: BigDecimal,
    termMonths: Int,
    startDate: LocalDate,
    extraMonthlyPayment: BigDecimal,
    securedBy: Option[AltAssetId],
    active: Boolean,
    createdAt: Instant
)

final case class LiabilityStatement(liabilityId: LiabilityId, date: LocalDate, balance: BigDecimal)

// ---------- Errors ----------

sealed trait PortfolioError
object PortfolioError {
  final case class AccountNotFound(id: InvestmentAccountId)                          extends PortfolioError
  final case class AssetNotFound(id: AssetId)                                        extends PortfolioError
  final case class DuplicateSymbol(symbol: Symbol)                                   extends PortfolioError
  final case class ActivityNotFound(id: ActivityId)                                  extends PortfolioError
  final case class InvalidActivity(reason: String)                                   extends PortfolioError
  final case class InsufficientQuantity(symbol: Symbol, held: BigDecimal, requested: BigDecimal, date: LocalDate) extends PortfolioError
  final case class CurrencyMismatch(expected: CurrencyCode, got: CurrencyCode)       extends PortfolioError
  final case class MissingFxRate(from: CurrencyCode, to: CurrencyCode)               extends PortfolioError
  final case class WrongTracking(id: InvestmentAccountId, expected: String)          extends PortfolioError
  final case class InvalidPeriod(from: LocalDate, to: LocalDate)                     extends PortfolioError
  final case class InvalidTargets(reason: String)                                    extends PortfolioError
  final case class GoalNotFound(id: GoalId)                                          extends PortfolioError
  final case class LimitNotFound(id: LimitId)                                        extends PortfolioError
  final case class AltAssetNotFound(id: AltAssetId)                                  extends PortfolioError
  final case class LiabilityNotFound(id: LiabilityId)                                extends PortfolioError
  final case class InvalidInput(reason: String)                                      extends PortfolioError
  final case class ImportFailed(errors: List[String])                                extends PortfolioError
}
