package moneystack.external

import moneystack.domain.*

import cats.effect.*
import cats.syntax.all.*

/**
 * Cloudpayments client. Replaces the legacy local PSPs flagged in the brief.
 *
 * The real API is webhook-driven: `charge` issues an authorization which is
 * later captured by an upstream callback. For the scaffold we expose synchronous
 * helpers and assume captures complete inline.
 */
trait PaymentGateway[F[_]] {

  /** Charge a previously-tokenised payment method for a one-off payment. */
  def chargeOnce(
      userEmail: Email,
      amount: PriceMinor,
      currency: CurrencyCode,
      description: String,
      idempotencyKey: String
  ): F[PaymentGateway.Result]

  /** Set up a recurring billing arrangement (subscription). Returns Cloudpayments token. */
  def startRecurring(
      userEmail: Email,
      amount: PriceMinor,
      currency: CurrencyCode,
      interval: BillingInterval,
      idempotencyKey: String
  ): F[PaymentGateway.RecurringSetup]

  def cancelRecurring(recurringRef: String): F[Unit]
  def refund(paymentRef: String, amount: PriceMinor): F[Unit]
}

object PaymentGateway {

  final case class Result(ref: String, status: PaymentStatus)
  final case class RecurringSetup(ref: String)

  /** Sandbox implementation — generates fake refs, marks everything captured. */
  def sandbox[F[_]: Sync]: F[PaymentGateway[F]] =
    Sync[F].pure(new PaymentGateway[F] {
      private def newRef = Sync[F].delay("cp_" + java.util.UUID.randomUUID().toString.take(20))

      def chargeOnce(userEmail: Email, amount: PriceMinor, currency: CurrencyCode,
                     description: String, idempotencyKey: String): F[Result] =
        newRef.map(ref => Result(ref, PaymentStatus.Captured))

      def startRecurring(userEmail: Email, amount: PriceMinor, currency: CurrencyCode,
                         interval: BillingInterval, idempotencyKey: String): F[RecurringSetup] =
        newRef.map(RecurringSetup(_))

      def cancelRecurring(ref: String): F[Unit] = Sync[F].unit
      def refund(ref: String, amount: PriceMinor): F[Unit] = Sync[F].unit
    })
}
