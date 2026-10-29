package wealth.db

import wealth.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait Subscriptions[F[_]] {
  def listPlans(): F[List[Plan]]
  def upsertPlan(p: Plan): F[Plan]
  def createSubscription(s: Subscription): F[Subscription]
  def updateStatus(id: SubscriptionId, status: SubscriptionStatus): F[Unit]
  def activeFor(userId: UserId): F[Option[Subscription]]
  def insertPayment(p: Payment): F[Payment]
  def listPaymentsFor(userId: UserId, limit: Int): F[List[Payment]]
}

object Subscriptions {

  import Codecs.{plan as planC, planId as planIdC, subscription as subC, subscriptionId as subIdC,
                 subscriptionStatus as subStatusC, userId as userIdC, payment as paymentC}
  import skunk.codec.all.int4

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Subscriptions[F] =
    new Subscriptions[F] {

      def listPlans(): F[List[Plan]] =
        pool.use(_.prepare(Q.listPlans).flatMap(_.stream(skunk.Void, 16).compile.toList))

      def upsertPlan(p: Plan): F[Plan] =
        pool.use(_.prepare(Q.upsertPlan).flatMap(_.unique(p)))

      def createSubscription(s: Subscription): F[Subscription] =
        pool.use(_.prepare(Q.insertSub).flatMap(_.unique(s)))

      def updateStatus(id: SubscriptionId, status: SubscriptionStatus): F[Unit] =
        pool.use(_.prepare(Q.setStatus).flatMap(_.execute((status, id)))).void

      def activeFor(userId: UserId): F[Option[Subscription]] =
        pool.use(_.prepare(Q.activeFor).flatMap(_.option(userId)))

      def insertPayment(p: Payment): F[Payment] =
        pool.use(_.prepare(Q.insertPayment).flatMap(_.unique(p)))

      def listPaymentsFor(userId: UserId, limit: Int): F[List[Payment]] =
        pool.use(_.prepare(Q.paymentsFor).flatMap(_.stream((userId, limit), 64).compile.toList))
    }

  private object Q {

    val listPlans: Query[skunk.Void, Plan] =
      sql"""SELECT id, code, name, price_minor, currency, interval, active
            FROM plans WHERE active ORDER BY price_minor""".query(planC)

    val upsertPlan: Query[Plan, Plan] =
      sql"""INSERT INTO plans (id, code, name, price_minor, currency, interval, active)
            VALUES ${planC.values}
            ON CONFLICT (code) DO UPDATE
              SET name = EXCLUDED.name, price_minor = EXCLUDED.price_minor,
                  currency = EXCLUDED.currency, interval = EXCLUDED.interval, active = EXCLUDED.active
            RETURNING id, code, name, price_minor, currency, interval, active""".query(planC)

    val insertSub: Query[Subscription, Subscription] =
      sql"""INSERT INTO subscriptions (id, user_id, plan_id, status,
                                       current_period_start, current_period_end, cancel_at, cloudpayments_ref)
            VALUES ${subC.values}
            RETURNING id, user_id, plan_id, status,
                      current_period_start, current_period_end, cancel_at, cloudpayments_ref""".query(subC)

    val setStatus: Command[(SubscriptionStatus, SubscriptionId)] =
      sql"UPDATE subscriptions SET status = $subStatusC WHERE id = $subIdC".command

    val activeFor: Query[UserId, Subscription] =
      sql"""SELECT id, user_id, plan_id, status,
                   current_period_start, current_period_end, cancel_at, cloudpayments_ref
            FROM subscriptions
            WHERE user_id = $userIdC AND status = 'active'
            ORDER BY current_period_end DESC LIMIT 1""".query(subC)

    val insertPayment: Query[Payment, Payment] =
      sql"""INSERT INTO payments (id, user_id, subscription_id, course_id, amount_minor, currency,
                                  status, cloudpayments_ref, created_at)
            VALUES ${paymentC.values}
            RETURNING id, user_id, subscription_id, course_id, amount_minor, currency,
                      status, cloudpayments_ref, created_at""".query(paymentC)

    val paymentsFor: Query[(UserId, Int), Payment] =
      sql"""SELECT id, user_id, subscription_id, course_id, amount_minor, currency,
                   status, cloudpayments_ref, created_at
            FROM payments WHERE user_id = $userIdC
            ORDER BY created_at DESC LIMIT $int4""".query(paymentC)

    val _ = planIdC // silence unused-import on planIdC
  }
}
