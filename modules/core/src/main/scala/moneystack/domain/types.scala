package moneystack.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import java.util.UUID

// ---------- Identifiers ----------

type UserId            = UserId.T
object UserId            extends RefinedType[UUID, Pure] {}

type PortfolioId       = PortfolioId.T
object PortfolioId       extends RefinedType[UUID, Pure] {}

type ModelPortfolioId  = ModelPortfolioId.T
object ModelPortfolioId  extends RefinedType[UUID, Pure] {}

type HoldingId         = HoldingId.T
object HoldingId         extends RefinedType[UUID, Pure] {}

type RecommendationId  = RecommendationId.T
object RecommendationId  extends RefinedType[UUID, Pure] {}

type SubscriptionId    = SubscriptionId.T
object SubscriptionId    extends RefinedType[UUID, Pure] {}

type PaymentId         = PaymentId.T
object PaymentId         extends RefinedType[UUID, Pure] {}

type PlanId            = PlanId.T
object PlanId            extends RefinedType[UUID, Pure] {}

type CourseId          = CourseId.T
object CourseId          extends RefinedType[UUID, Pure] {}

type WebinarId         = WebinarId.T
object WebinarId         extends RefinedType[UUID, Pure] {}

type EnrollmentId      = EnrollmentId.T
object EnrollmentId      extends RefinedType[UUID, Pure] {}

type NewsId            = NewsId.T
object NewsId            extends RefinedType[UUID, Pure] {}

type AlertId           = AlertId.T
object AlertId           extends RefinedType[UUID, Pure] {}

// ---------- People & contact ----------

type EmailConstraint = Not[Blank] & MaxLength[254] & Match["""^[^@\s]+@[^@\s]+\.[^@\s]+$"""]
type Email = Email.T
object Email extends RefinedType[String, EmailConstraint] {}

type DisplayName = DisplayName.T
object DisplayName extends RefinedType[String, Not[Blank] & MaxLength[120]] {}

type PasswordHash = PasswordHash.T
object PasswordHash extends RefinedType[String, Not[Blank] & MaxLength[120]] {}

/** Telegram chat id, signed Long. */
type TelegramChatId = TelegramChatId.T
object TelegramChatId extends RefinedType[Long, Pure] {}

// ---------- Market instruments ----------

/** Ticker symbol: 1..10 uppercase letters/digits/dots (e.g. BRK.B). */
type Ticker = Ticker.T
object Ticker extends RefinedType[String, Match["^[A-Z][A-Z0-9.]{0,9}$"]] {}

/** ISIN (12 chars: 2 letters + 9 alphanumerics + 1 check digit). */
type Isin = Isin.T
object Isin extends RefinedType[String, FixedLength[12] & Match["^[A-Z]{2}[A-Z0-9]{9}[0-9]$"]] {}

/** Exchange code (e.g. NASDAQ, NYSE, LSE). */
type Exchange = Exchange.T
object Exchange extends RefinedType[String, Not[Blank] & MaxLength[16]] {}

/** ISO-4217 currency code. */
type CurrencyCode = CurrencyCode.T
object CurrencyCode extends RefinedType[String, Match["^[A-Z]{3}$"]] {
  val USD: CurrencyCode = CurrencyCode.applyUnsafe("USD")
  val EUR: CurrencyCode = CurrencyCode.applyUnsafe("EUR")
  val GBP: CurrencyCode = CurrencyCode.applyUnsafe("GBP")
}

// ---------- Money & quantitative scalars ----------

/** Money in minor units. Sign carries meaning (-ve = outflow / loss). */
type AmountMinor = AmountMinor.T
object AmountMinor extends RefinedType[Long, Pure] {}

/** Strictly-positive amount (subscription prices, payment values). */
type PriceMinor = PriceMinor.T
object PriceMinor extends RefinedType[Long, Positive] {}

/**
 * Share quantity. BigDecimal because fractional shares are routine.
 * Enforced positive via the smart constructor (Iron's numeric constraints
 * don't compose cleanly with BigDecimal singletons).
 */
type Quantity = Quantity.T
object Quantity extends RefinedType[BigDecimal, Pure] {
  def positive(b: BigDecimal): Either[String, Quantity] =
    if (b > 0) Right(Quantity.assume(b)) else Left(s"quantity must be > 0: $b")
}

/** Per-unit price in instrument's currency. */
type UnitPrice = UnitPrice.T
object UnitPrice extends RefinedType[BigDecimal, Pure] {
  def positive(b: BigDecimal): Either[String, UnitPrice] =
    if (b > 0) Right(UnitPrice.assume(b)) else Left(s"price must be > 0: $b")
}

/** Basis points, 0..100_000 (= 0% .. 1000%). Used for allocation weights and bps yields. */
type Bps = Bps.T
object Bps extends RefinedType[Int, GreaterEqual[0] & LessEqual[100_000]] {}

/**
 * Allocation weight in basis points; an allocation table for a portfolio
 * must sum to 10_000 (= 100%). Stored as a refined Int because we want both
 * compile-time bounds and frictionless arithmetic.
 */
type WeightBps = WeightBps.T
object WeightBps extends RefinedType[Int, GreaterEqual[0] & LessEqual[10_000]] {}

/** Free-text description capped at a sensible length. */
type Body = Body.T
object Body extends RefinedType[String, Not[Blank] & MaxLength[8000]] {}

/** Headline / short title. */
type Title = Title.T
object Title extends RefinedType[String, Not[Blank] & MaxLength[200]] {}

// ---------- Domain enums (shared types) ----------

enum Locale {
  case EN, ES, FR, RU
}
object Locale {
  def parse(s: String): Either[String, Locale] = s.toLowerCase match {
    case "en" => Right(EN)
    case "es" => Right(ES)
    case "fr" => Right(FR)
    case "ru" => Right(RU)
    case o    => Left(s"unsupported locale: $o")
  }
  def render(l: Locale): String = l.toString.toLowerCase
}

enum Role {
  case Investor, Analyst, Accountant, Admin
}
object Role {
  def parse(s: String): Either[String, Role] = s.toLowerCase match {
    case "investor"   => Right(Investor)
    case "analyst"    => Right(Analyst)
    case "accountant" => Right(Accountant)
    case "admin"      => Right(Admin)
    case o            => Left(s"unknown role: $o")
  }
  def render(r: Role): String = r.toString.toLowerCase
}
