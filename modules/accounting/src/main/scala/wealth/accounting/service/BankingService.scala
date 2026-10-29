package wealth.accounting.service

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.db.{Banking, Contacts}
import wealth.platform.Db

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.Instant
import java.util.UUID

/**
 * Bank feeds: imported transactions are matched against rules, optionally
 * auto-categorised, and otherwise categorised (posted), excluded or restored
 * by hand.
 */
trait BankingService[F[_]] {
  def rules: F[List[BankRule]]
  def createRule(b: BankingService.NewRule): F[Either[LedgerError, BankRule]]
  def setRuleActive(id: BankRuleId, active: Boolean): F[Boolean]

  /** Skips rows whose `externalId` was already imported for the account. */
  def importTransactions(account: AccountId, rows: List[BankingService.NewTransaction], by: UserId): F[Either[LedgerError, BankingService.ImportResult]]
  def transactions(account: Option[AccountId], status: Option[BankTxStatus], limit: Int): F[List[BankTransaction]]
  /** Re-matches every uncategorised transaction against the current rules. */
  def runRules(by: UserId): F[BankingService.ImportResult]

  def categorize(id: BankTransactionId, account: AccountId, contact: Option[ContactId], by: UserId): F[Either[LedgerError, BankTransaction]]
  def uncategorize(id: BankTransactionId, by: UserId): F[Either[LedgerError, BankTransaction]]
  def exclude(id: BankTransactionId): F[Either[LedgerError, BankTransaction]]
  def restore(id: BankTransactionId): F[Either[LedgerError, BankTransaction]]
}

object BankingService {

  final case class NewRule(
      name: Title,
      priority: Option[Int],
      applyTo: RuleApplyTo,
      conjunction: Conjunction,
      conditions: List[RuleCondition],
      accountId: AccountId,
      contactId: Option[ContactId],
      autoCategorize: Boolean
  )

  final case class NewTransaction(date: java.time.LocalDate, description: Title, amountMinor: Long, externalId: Option[String])

  /** `failed` lists auto-categorisations that a rule matched but posting rejected (e.g. a locked period). */
  final case class ImportResult(imported: Int, duplicates: Int, matched: Int, categorized: Int, failed: List[(BankTransactionId, String)])

  def make[F[_]: Async](db: Db[F], posting: Posting[F], banking: Banking[F], contactRepo: Contacts[F]): BankingService[F] =
    new BankingService[F] {

      private type R[A] = EitherT[F, LedgerError, A]
      private def lift[A](fa: F[A]): R[A]                          = EitherT.liftF(fa)
      private def check(ok: Boolean, err: => LedgerError): R[Unit] = EitherT.cond[F](ok, (), err)

      def rules: F[List[BankRule]] = banking.rules()

      def createRule(b: NewRule): F[Either[LedgerError, BankRule]] =
        (posting.chart, b.contactId.flatTraverse(contactRepo.find)).tupled.flatMap { case (chart, contact) =>
          val checked = for {
            _ <- BankRules.validate(b.conditions)
            a <- chart.get(b.accountId).toRight(LedgerError.UnknownAccount(b.accountId))
            _ <- Either.cond(a.active, (), LedgerError.InactiveAccount(a.code))
            _ <- b.contactId.fold(Right(()))(id => contact.toRight(LedgerError.UnknownContact(id)).void)
          } yield ()
          checked.traverse { _ =>
            for {
              id  <- Async[F].delay(BankRuleId.assume(UUID.randomUUID()))
              now <- Async[F].delay(Instant.now())
              r   <- banking.createRule(BankRule(id, b.name, b.priority.getOrElse(100), b.applyTo, b.conjunction, b.conditions,
                                                 b.accountId, b.contactId, b.autoCategorize, active = true, now))
            } yield r
          }
        }

      def setRuleActive(id: BankRuleId, active: Boolean): F[Boolean] = banking.setRuleActive(id, active)

      def importTransactions(account: AccountId, rows: List[NewTransaction], by: UserId): F[Either[LedgerError, ImportResult]] =
        posting.chart.flatMap { chart =>
          val checked = for {
            a <- chart.get(account).toRight(LedgerError.UnknownAccount(account))
            _ <- Either.cond(a.accountType.isMoney, (), LedgerError.WrongAccountType(a.code, "cash, bank or credit card"))
            _ <- Either.cond(rows.forall(_.amountMinor != 0L), (), LedgerError.ZeroTotal)
          } yield ()
          checked.traverse { _ =>
            for {
              now     <- Async[F].delay(Instant.now())
              txs     <- rows.traverse(r => Async[F].delay(BankTransaction(
                           BankTransactionId.assume(UUID.randomUUID()), account, r.date, r.description, r.amountMinor,
                           r.externalId, BankTxStatus.Uncategorized, None, None, None, None, None, now)))
              fresh   <- banking.insertTransactions(txs)
              applied <- applyRules(fresh, by)
            } yield applied.copy(imported = fresh.size, duplicates = rows.size - fresh.size)
          }
        }

      def transactions(account: Option[AccountId], status: Option[BankTxStatus], limit: Int): F[List[BankTransaction]] =
        banking.transactions(account, status, limit)

      def runRules(by: UserId): F[ImportResult] =
        banking.transactions(None, Some(BankTxStatus.Uncategorized), 10_000).flatMap(applyRules(_, by))

      private def applyRules(txs: List[BankTransaction], by: UserId): F[ImportResult] =
        banking.rules().flatMap { rules =>
          txs.foldLeftM(ImportResult(0, 0, 0, 0, Nil)) { (acc, tx) =>
            BankRules.firstMatch(rules, tx) match {
              case None =>
                banking.setRule(tx.id, None).whenA(tx.ruleId.isDefined).as(acc)
              case Some(rule) =>
                banking.setRule(tx.id, Some(rule.id)) *> {
                  if (!rule.autoCategorize) Async[F].pure(acc.copy(matched = acc.matched + 1))
                  else categorize(tx.id, rule.accountId, rule.contactId, by).map {
                    case Right(_)  => acc.copy(matched = acc.matched + 1, categorized = acc.categorized + 1)
                    case Left(err) => acc.copy(matched = acc.matched + 1, failed = acc.failed :+ (tx.id -> err.toString))
                  }
                }
            }
          }
        }

      def categorize(id: BankTransactionId, account: AccountId, contact: Option[ContactId], by: UserId): F[Either[LedgerError, BankTransaction]] =
        posting.chart.flatMap { chart =>
          db.txE { s =>
            for {
              tx      <- EitherT.fromOptionF(banking.findForUpdate(s, id), LedgerError.BankTransactionNotFound(id): LedgerError)
              _       <- check(tx.status == BankTxStatus.Uncategorized, LedgerError.InvalidTransactionState(tx.status.key, "categorize"))
              bank    <- EitherT.fromOption[F](chart.get(tx.accountId), LedgerError.UnknownAccount(tx.accountId): LedgerError)
              rate    <- EitherT(posting.rate(bank.currency, None, tx.date))
              postId  <- lift(Async[F].delay(UUID.randomUUID()))
              header  <- lift(posting.header(tx.date, tx.externalId.flatMap(e => JournalRef.option(e.take(64))),
                                             Some(Body.assume(tx.description.value)), bank.currency, rate,
                                             JournalSource.BankTransaction, Some(postId), Some(by)))
              amount   = PriceMinor.assume(math.abs(tx.amountMinor))
              lines    = if (tx.isDeposit) List(JournalLine(tx.accountId, Side.Debit, amount, None), JournalLine(account, Side.Credit, amount, contact))
                         else List(JournalLine(account, Side.Debit, amount, contact), JournalLine(tx.accountId, Side.Credit, amount, None))
              entry   <- posting.postIn(s, header, lines, chart)
              updated  = tx.copy(status = BankTxStatus.Categorized, categoryAccountId = Some(account), contactId = contact,
                                 entryId = Some(entry.header.id), postingId = Some(postId))
              _       <- lift(banking.update(s, updated))
            } yield updated
          }
        }

      def uncategorize(id: BankTransactionId, by: UserId): F[Either[LedgerError, BankTransaction]] =
        posting.chart.flatMap { chart =>
          db.txE { s =>
            for {
              tx      <- EitherT.fromOptionF(banking.findForUpdate(s, id), LedgerError.BankTransactionNotFound(id): LedgerError)
              entryId <- EitherT.fromOption[F](tx.entryId.filter(_ => tx.status == BankTxStatus.Categorized),
                                               LedgerError.InvalidTransactionState(tx.status.key, "uncategorize"): LedgerError)
              _       <- posting.reverseIn(s, entryId, tx.date, Some(by), chart)
              updated  = tx.copy(status = BankTxStatus.Uncategorized, categoryAccountId = None, contactId = None,
                                 entryId = None, postingId = None)
              _       <- lift(banking.update(s, updated))
            } yield updated
          }
        }

      def exclude(id: BankTransactionId): F[Either[LedgerError, BankTransaction]] =
        transition(id, BankTxStatus.Uncategorized, BankTxStatus.Excluded, "exclude")

      def restore(id: BankTransactionId): F[Either[LedgerError, BankTransaction]] =
        transition(id, BankTxStatus.Excluded, BankTxStatus.Uncategorized, "restore")

      private def transition(id: BankTransactionId, from: BankTxStatus, to: BankTxStatus, action: String): F[Either[LedgerError, BankTransaction]] =
        db.txE { s =>
          for {
            tx      <- EitherT.fromOptionF(banking.findForUpdate(s, id), LedgerError.BankTransactionNotFound(id): LedgerError)
            _       <- check(tx.status == from, LedgerError.InvalidTransactionState(tx.status.key, action))
            updated  = tx.copy(status = to)
            _       <- lift(banking.update(s, updated))
          } yield updated
        }
    }
}
