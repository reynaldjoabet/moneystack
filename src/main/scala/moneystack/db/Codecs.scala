package moneystack.db

import moneystack.domain.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.skunk.*

import _root_.skunk.*
import _root_.skunk.codec.all.*

import java.time.ZoneOffset

object Codecs {

  // ---- identifier codecs ----
  val userId: Codec[UserId]                       = uuid.imap(UserId.assume)(_.value)
  val portfolioId: Codec[PortfolioId]             = uuid.imap(PortfolioId.assume)(_.value)
  val modelPortfolioId: Codec[ModelPortfolioId]   = uuid.imap(ModelPortfolioId.assume)(_.value)
  val holdingId: Codec[HoldingId]                 = uuid.imap(HoldingId.assume)(_.value)
  val recommendationId: Codec[RecommendationId]   = uuid.imap(RecommendationId.assume)(_.value)
  val subscriptionId: Codec[SubscriptionId]       = uuid.imap(SubscriptionId.assume)(_.value)
  val paymentId: Codec[PaymentId]                 = uuid.imap(PaymentId.assume)(_.value)
  val planId: Codec[PlanId]                       = uuid.imap(PlanId.assume)(_.value)
  val courseId: Codec[CourseId]                   = uuid.imap(CourseId.assume)(_.value)
  val webinarId: Codec[WebinarId]                 = uuid.imap(WebinarId.assume)(_.value)
  val enrollmentId: Codec[EnrollmentId]           = uuid.imap(EnrollmentId.assume)(_.value)
  val newsId: Codec[NewsId]                       = uuid.imap(NewsId.assume)(_.value)
  val alertId: Codec[AlertId]                     = uuid.imap(AlertId.assume)(_.value)

  // ---- scalars ----
  val email: Codec[Email]               = varchar(254).refined[EmailConstraint].imap(Email.assume)(_.value)
  val displayName: Codec[DisplayName]   = varchar(120).refined[Not[Blank] & MaxLength[120]].imap(DisplayName.assume)(_.value)
  val passwordHash: Codec[PasswordHash] = varchar(120).imap(PasswordHash.assume)(_.value)
  val telegramChat: Codec[TelegramChatId] = int8.imap(TelegramChatId.assume)(_.value)

  val ticker: Codec[Ticker]             = varchar(10).refined[Match["^[A-Z][A-Z0-9.]{0,9}$"]].imap(Ticker.assume)(_.value)
  val exchange: Codec[Exchange]         = varchar(16).refined[Not[Blank] & MaxLength[16]].imap(Exchange.assume)(_.value)
  val isin: Codec[Isin]                 = bpchar(12).refined[FixedLength[12] & Match["^[A-Z]{2}[A-Z0-9]{9}[0-9]$"]].imap(Isin.assume)(_.value)
  val currency: Codec[CurrencyCode]     = bpchar(3).refined[Match["^[A-Z]{3}$"]].imap(CurrencyCode.assume)(_.value)

  val title: Codec[Title]               = varchar(200).refined[Not[Blank] & MaxLength[200]].imap(Title.assume)(_.value)
  val body: Codec[Body]                 = text.refined[Not[Blank] & MaxLength[8000]].imap(Body.assume)(_.value)

  val amountMinor: Codec[AmountMinor]   = int8.imap(AmountMinor.assume)(_.value)
  val priceMinor: Codec[PriceMinor]     = int8.refined[Positive].imap(PriceMinor.assume)(_.value)
  val quantity: Codec[Quantity]         = numeric(38, 18).eimap[Quantity](Quantity.positive)(_.value)
  val unitPrice: Codec[UnitPrice]       = numeric(38, 18).eimap[UnitPrice](UnitPrice.positive)(_.value)
  val weightBps: Codec[WeightBps]       = int4.refined[GreaterEqual[0] & LessEqual[10_000]].imap(WeightBps.assume)(_.value)
  val bps: Codec[Bps]                   = int4.refined[GreaterEqual[0] & LessEqual[100_000]].imap(Bps.assume)(_.value)

  // ---- enums ----

  val locale: Codec[Locale] = varchar(2).eimap(Locale.parse)(Locale.render)
  val role: Codec[Role]     = varchar(16).eimap(Role.parse)(Role.render)

  val instrumentKind: Codec[InstrumentKind] =
    varchar(16).eimap(InstrumentKind.parse)(InstrumentKind.render)

  val portfolioStrategy: Codec[PortfolioStrategy] =
    varchar(24).eimap[PortfolioStrategy] {
      case "conservative" => Right(PortfolioStrategy.Conservative)
      case "balanced"     => Right(PortfolioStrategy.Balanced)
      case "growth"       => Right(PortfolioStrategy.Growth)
      case "aggressive"   => Right(PortfolioStrategy.Aggressive)
      case "income"       => Right(PortfolioStrategy.Income)
      case o              => Left(s"unknown strategy: $o")
    } { _.toString.toLowerCase }

  val tradeAction: Codec[TradeAction] =
    varchar(8).eimap[TradeAction] {
      case "buy"  => Right(TradeAction.Buy)
      case "sell" => Right(TradeAction.Sell)
      case "hold" => Right(TradeAction.Hold)
      case o      => Left(s"unknown action: $o")
    } { _.toString.toLowerCase }

  val billingInterval: Codec[BillingInterval] =
    varchar(16).eimap[BillingInterval] {
      case "monthly" => Right(BillingInterval.Monthly)
      case "annual"  => Right(BillingInterval.Annual)
      case o         => Left(s"unknown interval: $o")
    } { _.toString.toLowerCase }

  val subscriptionStatus: Codec[SubscriptionStatus] =
    varchar(16).eimap[SubscriptionStatus] {
      case "active"    => Right(SubscriptionStatus.Active)
      case "past_due"  => Right(SubscriptionStatus.PastDue)
      case "cancelled" => Right(SubscriptionStatus.Cancelled)
      case "expired"   => Right(SubscriptionStatus.Expired)
      case o           => Left(s"unknown subscription status: $o")
    } {
      case SubscriptionStatus.Active    => "active"
      case SubscriptionStatus.PastDue   => "past_due"
      case SubscriptionStatus.Cancelled => "cancelled"
      case SubscriptionStatus.Expired   => "expired"
    }

  val paymentStatus: Codec[PaymentStatus] =
    varchar(16).eimap[PaymentStatus] {
      case "authorised" => Right(PaymentStatus.Authorised)
      case "captured"   => Right(PaymentStatus.Captured)
      case "failed"     => Right(PaymentStatus.Failed)
      case "refunded"   => Right(PaymentStatus.Refunded)
      case o            => Left(s"unknown payment status: $o")
    } { _.toString.toLowerCase }

  private val instant: Codec[java.time.Instant] =
    timestamptz.imap(_.toInstant)(_.atOffset(ZoneOffset.UTC))

  // ---- aggregates ----

  val user: Codec[User] =
    (userId *: email *: passwordHash *: displayName *: role *: locale *: telegramChat.opt *: instant).to[User]

  val instrument: Codec[Instrument] =
    (ticker *: exchange *: isin.opt *: instrumentKind *: title *: currency).to[Instrument]

  val modelPortfolio: Codec[ModelPortfolio] =
    (modelPortfolioId *: title *: portfolioStrategy *: userId *: instant *: instant *: body.opt).to[ModelPortfolio]

  val modelAllocation: Codec[ModelAllocation] =
    (modelPortfolioId *: ticker *: exchange *: weightBps).to[ModelAllocation]

  val portfolio: Codec[Portfolio] =
    (portfolioId *: userId *: title *: currency *: modelPortfolioId.opt *: instant).to[Portfolio]

  val holding: Codec[Holding] =
    (holdingId *: portfolioId *: ticker *: exchange *: quantity *: unitPrice *: instant).to[Holding]

  val recommendation: Codec[Recommendation] =
    (recommendationId *: modelPortfolioId *: ticker *: exchange *: tradeAction *: body *:
      weightBps.opt *: instant *: userId).to[Recommendation]

  val plan: Codec[Plan] =
    (planId *: varchar(32) *: title *: priceMinor *: currency *: billingInterval *: bool).to[Plan]

  val subscription: Codec[Subscription] =
    (subscriptionId *: userId *: planId *: subscriptionStatus *: instant *: instant *:
      instant.opt *: varchar(128).opt).to[Subscription]

  val payment: Codec[Payment] =
    (paymentId *: userId *: subscriptionId.opt *: courseId.opt *: priceMinor *: currency *:
      paymentStatus *: varchar(128) *: instant).to[Payment]

  val course: Codec[Course] =
    (courseId *: title *: body *: priceMinor *: currency *: int4 *: instant).to[Course]

  val webinar: Codec[Webinar] =
    (webinarId *: title *: body *: instant *: instant *: varchar(512) *: userId).to[Webinar]
}
