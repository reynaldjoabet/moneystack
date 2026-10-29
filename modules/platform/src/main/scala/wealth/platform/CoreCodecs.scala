package wealth.platform

import wealth.domain.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.skunk.*

import _root_.skunk.*
import _root_.skunk.codec.all.*

import java.time.ZoneOffset

/** Skunk codecs for the shared `core` types, used by every module's own `Codecs`. */
object CoreCodecs {

  val userId: Codec[UserId]             = uuid.imap(UserId.assume)(_.value)
  val email: Codec[Email]               = varchar(254).refined[EmailConstraint].imap(Email.assume)(_.value)
  val title: Codec[Title]               = varchar(200).refined[Not[Blank] & MaxLength[200]].imap(Title.assume)(_.value)
  val body: Codec[Body]                 = text.refined[Not[Blank] & MaxLength[8000]].imap(Body.assume)(_.value)
  val currency: Codec[CurrencyCode]     = bpchar(3).refined[Match["^[A-Z]{3}$"]].imap(CurrencyCode.assume)(_.value)
  val amountMinor: Codec[AmountMinor]   = int8.imap(AmountMinor.assume)(_.value)
  val priceMinor: Codec[PriceMinor]     = int8.refined[Positive].imap(PriceMinor.assume)(_.value)
  val bps: Codec[Bps]                   = int4.refined[GreaterEqual[0] & LessEqual[100_000]].imap(Bps.assume)(_.value)

  /** Positive quantity in a NUMERIC(20, 6) column. */
  val quantity: Codec[Quantity]         = numeric(20, 6).eimap[Quantity](Quantity.positive)(_.value)

  /** Signed decimal in a NUMERIC(20, 6) column. */
  val decimal: Codec[BigDecimal]        = numeric(20, 6)

  /** Prices, rates and ratios in a NUMERIC(24, 10) column. */
  val fine: Codec[BigDecimal]           = numeric(24, 10)

  val instant: Codec[java.time.Instant] =
    timestamptz.imap(_.toInstant)(_.atOffset(ZoneOffset.UTC))

  /** A text column holding one of an enum's keys. */
  def enumOf[A](width: Int, parse: String => Either[String, A], render: A => String): Codec[A] =
    varchar(width).eimap(parse)(render)
}
