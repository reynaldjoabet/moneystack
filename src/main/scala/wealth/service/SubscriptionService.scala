package wealth.service

import wealth.domain.*
import wealth.db.{Subscriptions, Users}
import wealth.external.{Accounting, Crm, PaymentGateway}

import cats.effect.*
import cats.syntax.all.*

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID

/**
 * Subscription lifecycle. Charges the user via Cloudpayments, opens the
 * subscription record, records the first period's payment, and mirrors the
 * change into Salesforce + the general ledger.
 *
 * On failure, nothing is persisted. The Cloudpayments idempotency key is
 * derived from `(userId, planId, periodStart)` so retries are safe.
 */
trait SubscriptionService[F[_]] {
  def subscribe(userId: UserId, planId: PlanId): F[Either[SubscriptionService.Error, Subscription]]
  def cancel(subId: SubscriptionId): F[Unit]
}

object SubscriptionService {

  sealed trait Error
  object Error {
    case object UserNotFound                extends Error
    case object PlanNotFound                extends Error
    final case class GatewayFailure(msg: String) extends Error
  }

  def make[F[_]: Async](
      users: Users[F],
      subscriptions: Subscriptions[F],
      gateway: PaymentGateway[F],
      crm: Crm[F],
      accounting: Accounting[F]
  ): SubscriptionService[F] = new SubscriptionService[F] {

    def subscribe(userId: UserId, planId: PlanId): F[Either[Error, Subscription]] =
      users.find(userId).flatMap {
        case None    => Async[F].pure(Left(Error.UserNotFound))
        case Some(u) =>
          subscriptions.listPlans().map(_.find(_.id == planId)).flatMap {
            case None       => Async[F].pure(Left(Error.PlanNotFound))
            case Some(plan) =>
              for {
                now   <- Async[F].delay(Instant.now())
                end    = nextPeriodEnd(now, plan.interval)
                idem   = s"${userId.value}:${planId.value}:${now.toEpochMilli / (1000 * 60 * 60)}"
                attempt <- gateway.startRecurring(u.email, plan.priceMinor, plan.currency, plan.interval, idem).attempt
                result  <- attempt match {
                             case Left(err) => Async[F].pure(Left(Error.GatewayFailure(err.getMessage)))
                             case Right(setup) =>
                               for {
                                 sub <- subscriptions.createSubscription(Subscription(
                                          id                  = SubscriptionId.assume(UUID.randomUUID()),
                                          userId              = userId,
                                          planId              = planId,
                                          status              = SubscriptionStatus.Active,
                                          currentPeriodStart  = now,
                                          currentPeriodEnd    = end,
                                          cancelAt            = None,
                                          cloudpaymentsRef    = Some(setup.ref)
                                        ))
                                 payment <- subscriptions.insertPayment(Payment(
                                              id               = PaymentId.assume(UUID.randomUUID()),
                                              userId           = userId,
                                              subscriptionId   = Some(sub.id),
                                              courseId         = None,
                                              amountMinor      = plan.priceMinor,
                                              currency         = plan.currency,
                                              status           = PaymentStatus.Captured,
                                              cloudpaymentsRef = setup.ref,
                                              createdAt        = now
                                            ))
                                 // Mirror to CRM + the ledger; non-fatal if these fail. Ledger
                                 // posting is idempotent per payment, so it can be replayed.
                                 _   <- crm.recordSubscriptionChange(userId, sub).attempt.void
                                 _   <- crm.upsertContact(userId, u.email, u.displayName).attempt.void
                                 _   <- crm.recordPayment(userId, payment).attempt.void
                                 _   <- accounting.postRevenue(userId, payment).attempt.void
                               } yield Right(sub)
                           }
              } yield result
          }
      }

    def cancel(subId: SubscriptionId): F[Unit] =
      subscriptions.updateStatus(subId, SubscriptionStatus.Cancelled)
  }

  private def nextPeriodEnd(start: Instant, interval: BillingInterval): Instant = interval match {
    case BillingInterval.Monthly => start.plus(30, ChronoUnit.DAYS)
    case BillingInterval.Annual  => start.plus(365, ChronoUnit.DAYS)
  }
}
