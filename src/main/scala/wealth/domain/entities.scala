package wealth.domain

import java.time.{Instant, LocalDate, LocalDateTime}

// ---------- User & profile ----------

final case class User(
    id: UserId,
    email: Email,
    passwordHash: PasswordHash,
    displayName: DisplayName,
    role: Role,
    locale: Locale,
    telegramChatId: Option[TelegramChatId],
    createdAt: Instant
)

// ---------- Instruments ----------

enum InstrumentKind {
  case Equity, Etf, Bond, MutualFund, Crypto, Cash
}
object InstrumentKind {
  def parse(s: String): Either[String, InstrumentKind] = s.toLowerCase match {
    case "equity"      => Right(Equity)
    case "etf"         => Right(Etf)
    case "bond"        => Right(Bond)
    case "mutual_fund" => Right(MutualFund)
    case "crypto"      => Right(Crypto)
    case "cash"        => Right(Cash)
    case o             => Left(s"unknown instrument: $o")
  }
  def render(k: InstrumentKind): String = k match {
    case Equity     => "equity"
    case Etf        => "etf"
    case Bond       => "bond"
    case MutualFund => "mutual_fund"
    case Crypto     => "crypto"
    case Cash       => "cash"
  }
}

/** Instrument reference data. Composite identity is `(ticker, exchange)`. */
final case class Instrument(
    ticker: Ticker,
    exchange: Exchange,
    isin: Option[Isin],
    kind: InstrumentKind,
    name: Title,
    currency: CurrencyCode
)

// ---------- Portfolios ----------

enum PortfolioStrategy {
  case Conservative, Balanced, Growth, Aggressive, Income
}

/**
 * A model portfolio — curated allocation maintained by the analytics team.
 * Investors can subscribe to / clone these.
 */
final case class ModelPortfolio(
    id: ModelPortfolioId,
    name: Title,
    strategy: PortfolioStrategy,
    createdBy: UserId,
    createdAt: Instant,
    updatedAt: Instant,
    description: Option[Body]
)

/** A weighted target allocation row within a model portfolio. Sum of all rows
  * for one portfolio must equal 10_000 bps (enforced in the service layer). */
final case class ModelAllocation(
    modelPortfolioId: ModelPortfolioId,
    ticker: Ticker,
    exchange: Exchange,
    weightBps: WeightBps
)

/** An investor's actual portfolio — tied to a user, optionally backed by a model. */
final case class Portfolio(
    id: PortfolioId,
    userId: UserId,
    name: Title,
    baseCurrency: CurrencyCode,
    backedBy: Option[ModelPortfolioId],
    createdAt: Instant
)

/** A single position within an investor's portfolio. */
final case class Holding(
    id: HoldingId,
    portfolioId: PortfolioId,
    ticker: Ticker,
    exchange: Exchange,
    quantity: Quantity,
    avgCostUnit: UnitPrice,
    updatedAt: Instant
)

// ---------- Trade recommendations ----------

enum TradeAction { case Buy, Sell, Hold }

final case class Recommendation(
    id: RecommendationId,
    modelPortfolioId: ModelPortfolioId,
    ticker: Ticker,
    exchange: Exchange,
    action: TradeAction,
    rationale: Body,
    targetWeightBps: Option[WeightBps],
    publishedAt: Instant,
    publishedBy: UserId
)

// ---------- Market data snapshots ----------

final case class Quote(
    ticker: Ticker,
    exchange: Exchange,
    price: UnitPrice,
    currency: CurrencyCode,
    asOf: Instant,
    source: String                     // "alpha_vantage" | "yahoo" | "bloomberg"
)

final case class NewsItem(
    id: NewsId,
    title: Title,
    body: Body,
    tickers: List[Ticker],             // tagged tickers for filtering
    publishedAt: Instant,
    authorId: Option[UserId],          // None if syndicated from a feed
    source: String
)

final case class Alert(
    id: AlertId,
    userId: UserId,
    kind: String,                      // 'price_threshold' | 'pct_move' | 'news_tag' | ...
    payload: String,                   // JSON-encoded parameters
    deliveredAt: Option[Instant]
)

// ---------- Subscriptions & billing ----------

enum BillingInterval { case Monthly, Annual }

final case class Plan(
    id: PlanId,
    code: String,
    name: Title,
    priceMinor: PriceMinor,
    currency: CurrencyCode,
    interval: BillingInterval,
    active: Boolean
)

enum SubscriptionStatus { case Active, PastDue, Cancelled, Expired }

final case class Subscription(
    id: SubscriptionId,
    userId: UserId,
    planId: PlanId,
    status: SubscriptionStatus,
    currentPeriodStart: Instant,
    currentPeriodEnd: Instant,
    cancelAt: Option[Instant],
    cloudpaymentsRef: Option[String]
)

enum PaymentStatus { case Authorised, Captured, Failed, Refunded }

final case class Payment(
    id: PaymentId,
    userId: UserId,
    subscriptionId: Option[SubscriptionId],
    courseId: Option[CourseId],
    amountMinor: PriceMinor,
    currency: CurrencyCode,
    status: PaymentStatus,
    cloudpaymentsRef: String,
    createdAt: Instant
)

// ---------- Learning ----------

final case class Course(
    id: CourseId,
    title: Title,
    description: Body,
    priceMinor: PriceMinor,
    currency: CurrencyCode,
    minutes: Int,                       // total runtime
    publishedAt: Instant
)

final case class Webinar(
    id: WebinarId,
    title: Title,
    description: Body,
    startsAt: Instant,
    endsAt: Instant,
    streamUrl: String,
    hostId: UserId
)

final case class Enrollment(
    id: EnrollmentId,
    userId: UserId,
    courseId: Option[CourseId],
    webinarId: Option[WebinarId],
    enrolledAt: Instant
)

// ---------- Performance reporting ----------

final case class PortfolioPerformance(
    portfolioId: PortfolioId,
    asOf: LocalDate,
    marketValueMinor: AmountMinor,
    costBasisMinor: AmountMinor,
    unrealizedPnLMinor: AmountMinor,
    twrBpsToDate: Bps                   // time-weighted return since inception, in bps
)
