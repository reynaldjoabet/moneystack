package moneystack.service

import moneystack.domain.*
import moneystack.db.Users
import moneystack.external.Accounting
import moneystack.accounting.AccountingModule
import moneystack.accounting.domain.{ContactKind, JournalLine, JournalRef, JournalSource, Side}
import moneystack.accounting.service.LedgerService
import moneystack.accounting.service.LedgerService.SystemAccounts

import cats.effect.*
import cats.syntax.all.*

import java.time.{LocalDate, YearMonth, ZoneOffset}

/**
 * [[Accounting]] backed by the accounting module, in place of the FinancialForce
 * sandbox. Each paying user gets a customer contact; a captured payment posts
 * Cloudpayments clearing / subscription or course revenue, once per payment.
 */
object LedgerAccounting {

  def make[F[_]: Sync](users: Users[F], accounting: AccountingModule[F]): Accounting[F] = new Accounting[F] {

    def postRevenue(userId: UserId, payment: Payment): F[Unit] =
      if (payment.status != PaymentStatus.Captured) Sync[F].unit
      else
        for {
          user    <- users.find(userId).flatMap(Sync[F].fromOption(_, new IllegalStateException(s"unknown user ${userId.value}")))
          contact <- accounting.catalog.contactForUser(ContactKind.Customer, userId, Title.assume(user.displayName.value),
                                                       Some(user.email), payment.currency)
          revenue  = if (payment.courseId.isDefined) SystemAccounts.CourseRevenue else SystemAccounts.SubscriptionRevenue
          draft    = LedgerService.Draft(
                       date         = LocalDate.ofInstant(payment.createdAt, ZoneOffset.UTC),
                       reference    = JournalRef.option(payment.cloudpaymentsRef.take(64)),
                       memo         = None,
                       currency     = payment.currency,
                       exchangeRate = None,
                       lines        = List(
                         JournalLine(SystemAccounts.PaymentClearing.id, Side.Debit, payment.amountMinor, Some(contact.id)),
                         JournalLine(revenue.id, Side.Credit, payment.amountMinor, Some(contact.id))
                       )
                     )
          res     <- accounting.ledger.postAutomated(draft, JournalSource.Payment, payment.id.value)
          _       <- res.leftMap(err => new IllegalStateException(s"ledger rejected payment ${payment.id.value}: $err")).liftTo[F]
        } yield ()

    def revenueReport(year: Int, month: Int): F[List[(UserId, PriceMinor)]] = {
      val ym = YearMonth.of(year, month)
      accounting.ledger.revenueByUser(ym.atDay(1), ym.atEndOfMonth)
        .map(_.flatMap { case (user, minor) => PriceMinor.option(minor).map(user -> _) })
    }
  }
}
