package moneystack.http

import moneystack.domain.*
import moneystack.service.PortfolioService

import io.circe.*
import io.circe.generic.semiauto.*
import io.github.iltotore.iron.circe.given

object Json {

  // ---- enum encoders ----
  given Encoder[Locale]            = Encoder.encodeString.contramap(Locale.render)
  given Decoder[Locale]            = Decoder.decodeString.emap(Locale.parse)
  given Encoder[Role]              = Encoder.encodeString.contramap(Role.render)
  given Decoder[Role]              = Decoder.decodeString.emap(Role.parse)
  given Encoder[InstrumentKind]    = Encoder.encodeString.contramap(InstrumentKind.render)
  given Decoder[InstrumentKind]    = Decoder.decodeString.emap(InstrumentKind.parse)
  given Encoder[PortfolioStrategy] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  given Decoder[PortfolioStrategy] = Decoder.decodeString.emap {
    case "conservative" => Right(PortfolioStrategy.Conservative)
    case "balanced"     => Right(PortfolioStrategy.Balanced)
    case "growth"       => Right(PortfolioStrategy.Growth)
    case "aggressive"   => Right(PortfolioStrategy.Aggressive)
    case "income"       => Right(PortfolioStrategy.Income)
    case o              => Left(s"unknown strategy: $o")
  }
  given Encoder[TradeAction] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  given Decoder[TradeAction] = Decoder.decodeString.emap {
    case "buy"  => Right(TradeAction.Buy)
    case "sell" => Right(TradeAction.Sell)
    case "hold" => Right(TradeAction.Hold)
    case o      => Left(s"unknown action: $o")
  }
  given Encoder[BillingInterval]    = Encoder.encodeString.contramap(_.toString.toLowerCase)
  given Encoder[SubscriptionStatus] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  given Encoder[PaymentStatus]      = Encoder.encodeString.contramap(_.toString.toLowerCase)

  // ---- aggregate encoders (never serialise password hashes) ----
  given Encoder[User] = Encoder.instance { u =>
    io.circe.Json.obj(
      "id"          -> Encoder.encodeString.apply(u.id.value.toString),
      "email"       -> Encoder.encodeString.apply(u.email.value),
      "displayName" -> Encoder.encodeString.apply(u.displayName.value),
      "role"        -> Encoder[Role].apply(u.role),
      "locale"      -> Encoder[Locale].apply(u.locale),
      "createdAt"   -> Encoder[java.time.Instant].apply(u.createdAt)
    )
  }
  val userEncoder: Encoder[User] = summon[Encoder[User]]

  given Encoder[Instrument]           = deriveEncoder
  given Encoder[ModelPortfolio]       = deriveEncoder
  given Encoder[ModelAllocation]      = deriveEncoder
  given Encoder[Portfolio]            = deriveEncoder
  given Encoder[Holding]              = deriveEncoder
  given Encoder[Recommendation]       = deriveEncoder
  given Encoder[Quote]                = deriveEncoder
  given Encoder[NewsItem]             = deriveEncoder
  given Encoder[Plan]                 = deriveEncoder
  given Encoder[Subscription]         = deriveEncoder
  given Encoder[Payment]              = deriveEncoder
  given Encoder[Course]               = deriveEncoder
  given Encoder[Webinar]              = deriveEncoder
  given Encoder[PortfolioPerformance] = deriveEncoder

  given Encoder[PortfolioService.ValuationLine] = deriveEncoder
  given Encoder[PortfolioService.Valuation]     = deriveEncoder

  // ---- request bodies ----
  final case class SignupBody(email: Email, password: String, displayName: DisplayName, role: Role, locale: Locale)
  given Decoder[SignupBody] = deriveDecoder

  final case class LoginBody(email: Email, password: String)
  given Decoder[LoginBody] = deriveDecoder

  final case class CreatePortfolioBody(name: Title, baseCurrency: CurrencyCode, backedBy: Option[ModelPortfolioId])
  given Decoder[CreatePortfolioBody] = deriveDecoder

  final case class UpsertHoldingBody(ticker: Ticker, exchange: Exchange, quantity: Quantity, avgCostUnit: UnitPrice)
  given Decoder[UpsertHoldingBody] = deriveDecoder

  final case class CreateModelBody(name: Title, strategy: PortfolioStrategy, description: Option[Body], allocations: List[ModelAllocation])
  given Decoder[ModelAllocation] = deriveDecoder
  given Decoder[CreateModelBody] = deriveDecoder

  final case class PublishRecBody(
      modelId: ModelPortfolioId,
      ticker: Ticker,
      exchange: Exchange,
      action: TradeAction,
      rationale: Body,
      targetWeightBps: Option[WeightBps]
  )
  given Decoder[PublishRecBody] = deriveDecoder

  final case class SubscribeBody(planId: PlanId)
  given Decoder[SubscribeBody] = deriveDecoder

  final case class UpsertPlanBody(
      code: String,
      name: Title,
      priceMinor: PriceMinor,
      currency: CurrencyCode,
      interval: BillingInterval,
      active: Boolean
  )
  given Decoder[BillingInterval] = Decoder.decodeString.emap {
    case "monthly" => Right(BillingInterval.Monthly)
    case "annual"  => Right(BillingInterval.Annual)
    case o         => Left(s"unknown interval: $o")
  }
  given Decoder[UpsertPlanBody] = deriveDecoder
}
