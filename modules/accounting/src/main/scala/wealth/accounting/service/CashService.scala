package wealth.accounting.service

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.db.Office
import wealth.platform.Db

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Money that moves without a document, after Akaunting's banking screens:
 * direct income and expenses, transfers between money accounts, and
 * reconciling a money account against its statement.
 */
trait CashService[F[_]] {
  def moneyIn(b: CashService.NewMovement, by: UserId): F[Either[LedgerError, JournalEntry]]
  def moneyOut(b: CashService.NewMovement, by: UserId): F[Either[LedgerError, JournalEntry]]
  def transfer(b: CashService.NewTransfer, by: UserId): F[Either[LedgerError, JournalEntry]]

  def startReconciliation(b: CashService.NewReconciliation, by: UserId): F[Either[LedgerError, ReconciliationView]]
  def reconciliation(id: ReconciliationId): F[Either[LedgerError, ReconciliationView]]
  def reconciliations(account: AccountId): F[List[Reconciliation]]
  def setCleared(id: ReconciliationId, lines: List[(JournalEntryId, Int)], cleared: Boolean): F[Either[LedgerError, ReconciliationView]]
  /** Closes the reconciliation once the ticked lines explain the statement exactly. */
  def closeReconciliation(id: ReconciliationId): F[Either[LedgerError, ReconciliationView]]
}

object CashService {

  /**
   * Money into (or out of) `accountId`, a cash, bank or card account, against
   * `counterAccountId` (income for money in, expense for money out, or any
   * other non-money account). `currency` defaults to the money account's.
   */
  final case class NewMovement(
      accountId: AccountId,
      counterAccountId: AccountId,
      amountMinor: PriceMinor,
      date: LocalDate,
      currency: Option[CurrencyCode],
      exchangeRate: Option[ExchangeRate],
      contactId: Option[ContactId],
      reference: Option[JournalRef],
      memo: Option[Body]
  )

  /** Posted in the source account's currency; the destination must share it or be in base currency. */
  final case class NewTransfer(fromAccountId: AccountId, toAccountId: AccountId, amountMinor: PriceMinor, date: LocalDate,
                               exchangeRate: Option[ExchangeRate], reference: Option[JournalRef], memo: Option[Body])

  final case class NewReconciliation(accountId: AccountId, periodStart: LocalDate, periodEnd: LocalDate, closingMinor: Long)

  def make[F[_]: Async](db: Db[F], posting: Posting[F], contacts: wealth.accounting.db.Contacts[F], office: Office[F]): CashService[F] =
    new CashService[F] {

      private type R[A] = EitherT[F, LedgerError, A]
      private def lift[A](fa: F[A]): R[A] = EitherT.liftF(fa)
      private def check(ok: Boolean, err: => LedgerError): R[Unit] = EitherT.cond[F](ok, (), err)

      private def account(chart: Map[AccountId, Account], id: AccountId): R[Account] =
        EitherT.fromOption[F](chart.get(id), LedgerError.UnknownAccount(id): LedgerError)

      private def money(chart: Map[AccountId, Account], id: AccountId): R[Account] =
        account(chart, id).flatMap(a => check(a.accountType.isMoney, LedgerError.WrongAccountType(a.code, "cash, bank or credit card")).as(a))

      private def movement(b: NewMovement, by: UserId, inbound: Boolean): F[Either[LedgerError, JournalEntry]] =
        (for {
          chart   <- lift(posting.chart)
          cash    <- money(chart, b.accountId)
          counter <- account(chart, b.counterAccountId)
          _       <- check(!counter.accountType.isMoney, LedgerError.WrongAccountType(counter.code, "a non-money account (use a transfer between money accounts)"))
          _       <- b.contactId.traverse_(c => EitherT.fromOptionF(contacts.find(c), LedgerError.UnknownContact(c): LedgerError))
          ccy      = b.currency.getOrElse(cash.currency)
          rate    <- EitherT(posting.rate(ccy, b.exchangeRate, b.date))
          header  <- lift(posting.header(b.date, b.reference, b.memo, ccy, rate,
                                         if (inbound) JournalSource.MoneyIn else JournalSource.MoneyOut, None, Some(by)))
          lines    = if (inbound) List(JournalLine(cash.id, Side.Debit, b.amountMinor, None), JournalLine(counter.id, Side.Credit, b.amountMinor, b.contactId))
                     else List(JournalLine(counter.id, Side.Debit, b.amountMinor, b.contactId), JournalLine(cash.id, Side.Credit, b.amountMinor, None))
          entry   <- EitherT(db.txE[LedgerError, JournalEntry](s => posting.postIn(s, header, lines, chart)))
        } yield entry).value

      def moneyIn(b: NewMovement, by: UserId): F[Either[LedgerError, JournalEntry]]  = movement(b, by, inbound = true)
      def moneyOut(b: NewMovement, by: UserId): F[Either[LedgerError, JournalEntry]] = movement(b, by, inbound = false)

      def transfer(b: NewTransfer, by: UserId): F[Either[LedgerError, JournalEntry]] =
        (for {
          chart  <- lift(posting.chart)
          from   <- money(chart, b.fromAccountId)
          to     <- money(chart, b.toAccountId)
          _      <- check(from.id != to.id, LedgerError.InvalidDraft("a transfer needs two different accounts"))
          rate   <- EitherT(posting.rate(from.currency, b.exchangeRate, b.date))
          header <- lift(posting.header(b.date, b.reference, b.memo, from.currency, rate, JournalSource.Transfer, None, Some(by)))
          entry  <- EitherT(db.txE[LedgerError, JournalEntry](s => posting.postIn(s, header,
                      List(JournalLine(to.id, Side.Debit, b.amountMinor, None), JournalLine(from.id, Side.Credit, b.amountMinor, None)), chart)))
        } yield entry).value

      // ---- reconciliation ----

      /** Lines in the account's own currency, signed so the usual balance is positive. */
      private def view(r: Reconciliation): F[Either[LedgerError, ReconciliationView]] =
        posting.chart.flatMap { chart =>
          chart.get(r.accountId) match {
            case None => Async[F].pure(Left(LedgerError.UnknownAccount(r.accountId)))
            case Some(acct) =>
              office.accountLines(r).map { raw =>
                val lines = raw.map { l =>
                  val amount = if (acct.currency == posting.baseCurrency) l.baseAmountMinor else l.amountMinor
                  val signed = if (l.side == Side.Debit) Ledger.balance(acct.accountType, amount, 0) else Ledger.balance(acct.accountType, 0, amount)
                  ReconLine(l.entryId, l.lineNo, l.date, l.reference, l.memo, signed, l.cleared)
                }
                val (cleared, diff) = Reconcile.difference(r, lines)
                Right(ReconciliationView(r, lines, cleared, diff))
              }
          }
        }

      def startReconciliation(b: NewReconciliation, by: UserId): F[Either[LedgerError, ReconciliationView]] =
        (for {
          chart <- lift(posting.chart)
          _     <- money(chart, b.accountId)
          _     <- check(!b.periodEnd.isBefore(b.periodStart), LedgerError.InvalidPeriod(b.periodStart, b.periodEnd))
          open  <- lift(office.reconciliations(b.accountId)).map(_.exists(_.status == ReconciliationStatus.Open))
          _     <- check(!open, LedgerError.InvalidDraft("finish the open reconciliation on this account first"))
          last  <- lift(office.lastClosed(b.accountId))
          _     <- check(last.forall(_.periodEnd.isBefore(b.periodStart)), LedgerError.InvalidPeriod(b.periodStart, b.periodEnd))
          id    <- lift(Async[F].delay(ReconciliationId.assume(UUID.randomUUID())))
          now   <- lift(Async[F].delay(Instant.now()))
          r     <- lift(office.createReconciliation(Reconciliation(id, b.accountId, b.periodStart, b.periodEnd, last.fold(0L)(_.closingMinor),
                                                               b.closingMinor, ReconciliationStatus.Open, by, now, None)))
          v     <- EitherT(view(r))
        } yield v).value

      private def find(id: ReconciliationId): R[Reconciliation] =
        EitherT.fromOptionF(office.reconciliation(id), LedgerError.ReconciliationNotFound(id): LedgerError)

      def reconciliation(id: ReconciliationId): F[Either[LedgerError, ReconciliationView]] =
        (find(id).flatMap(r => EitherT(view(r)))).value

      def reconciliations(account: AccountId): F[List[Reconciliation]] = office.reconciliations(account)

      def setCleared(id: ReconciliationId, lines: List[(JournalEntryId, Int)], cleared: Boolean): F[Either[LedgerError, ReconciliationView]] =
        (for {
          r       <- find(id)
          _       <- check(r.status == ReconciliationStatus.Open, LedgerError.ReconciliationClosed)
          current <- EitherT(view(r))
          _       <- lines.traverse_ { case (e, n) =>
                       check(current.lines.exists(l => l.entryId == e && l.lineNo == n), LedgerError.LineNotOnAccount(e, n))
                     }
          _       <- lift(office.setCleared(id, lines, cleared))
          v       <- EitherT(view(r))
        } yield v).value

      def closeReconciliation(id: ReconciliationId): F[Either[LedgerError, ReconciliationView]] =
        (for {
          r   <- find(id)
          _   <- check(r.status == ReconciliationStatus.Open, LedgerError.ReconciliationClosed)
          v   <- EitherT(view(r))
          _   <- check(v.differenceMinor == 0L, LedgerError.ReconciliationUnbalanced(v.differenceMinor))
          now <- lift(Async[F].delay(Instant.now()))
          _   <- lift(office.close(id, now))
        } yield v.copy(reconciliation = r.copy(status = ReconciliationStatus.Closed, closedAt = Some(now)))).value
    }
}
