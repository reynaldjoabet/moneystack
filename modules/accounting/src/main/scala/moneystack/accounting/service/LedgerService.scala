package moneystack.accounting.service

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.db.{ExchangeRates, Journals, LedgerAccounts}
import moneystack.platform.Db

import cats.effect.*
import cats.syntax.all.*

import java.nio.charset.StandardCharsets.UTF_8
import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Double-entry general ledger: chart of accounts, balanced journal entries in
 * any currency (valued in the base currency at a recorded rate), reversals,
 * period locking, and the core statements.
 */
trait LedgerService[F[_]] {

  /** Creates the system accounts that automated postings rely on. Idempotent. */
  def bootstrap: F[Unit]

  def accounts: F[List[Account]]
  def createAccount(body: LedgerService.NewAccount): F[Either[LedgerError, Account]]
  def setActive(id: AccountId, active: Boolean): F[Option[Account]]

  def post(draft: LedgerService.Draft, by: UserId): F[Either[LedgerError, JournalEntry]]
  def reverse(id: JournalEntryId, date: LocalDate, by: UserId): F[Either[LedgerError, JournalEntry]]

  /** Posts on behalf of another system; re-posting the same `(source, sourceId)` returns the original entry. */
  def postAutomated(draft: LedgerService.Draft, source: JournalSource, sourceId: UUID): F[Either[LedgerError, JournalEntry]]
  def entry(id: JournalEntryId): F[Option[JournalEntry]]
  def entries(from: LocalDate, to: LocalDate, limit: Int): F[List[JournalEntry]]

  def setRate(r: FxRate): F[Unit]
  def rates(currency: CurrencyCode): F[List[FxRate]]

  def lock: F[Option[LedgerLock]]
  def setLock(l: LedgerLock): F[Unit]
  def clearLock: F[Unit]

  def trialBalance(asOf: LocalDate): F[FinancialReports.TrialBalance]
  def profitAndLoss(from: LocalDate, to: LocalDate): F[Either[LedgerError, FinancialReports.ProfitAndLoss]]
  def balanceSheet(asOf: LocalDate): F[FinancialReports.BalanceSheet]
  def generalLedger(from: LocalDate, to: LocalDate, account: Option[AccountId]): F[Either[LedgerError, FinancialReports.GeneralLedger]]
  def revenueByUser(from: LocalDate, to: LocalDate): F[List[(UserId, Long)]]
}

object LedgerService {

  final case class NewAccount(
      code: AccountCode,
      name: Title,
      accountType: AccountType,
      currency: Option[CurrencyCode],
      parentId: Option[AccountId]
  )

  /** `exchangeRate` overrides the stored daily rate; ignored for base-currency entries. */
  final case class Draft(
      date: LocalDate,
      reference: Option[JournalRef],
      memo: Option[Body],
      currency: CurrencyCode,
      exchangeRate: Option[ExchangeRate],
      lines: List[JournalLine]
  )

  /** Accounts created by [[LedgerService.bootstrap]]; ids are derived from the code so they're stable. */
  final case class SystemAccount(code: AccountCode, name: Title, accountType: AccountType) {
    // "wealth." is a fixed namespace from the project's first name: changing it would change existing ids.
    val id: AccountId = AccountId.assume(UUID.nameUUIDFromBytes(s"wealth.ledger.${code.value}".getBytes(UTF_8)))
  }

  object SystemAccounts {
    private def acct(code: String, name: String, t: AccountType) =
      SystemAccount(AccountCode.applyUnsafe(code), Title.applyUnsafe(name), t)

    val Cash                 = acct("1000", "Cash", AccountType.Cash)
    val PaymentClearing      = acct("1010", "Cloudpayments clearing", AccountType.Bank)
    val Receivables          = acct("1100", "Accounts receivable", AccountType.AccountsReceivable)
    val InputTax             = acct("1150", "Input tax recoverable", AccountType.OtherCurrentAsset)
    val Inventory            = acct("1300", "Inventory", AccountType.Inventory)
    val Payables             = acct("2000", "Accounts payable", AccountType.AccountsPayable)
    val TaxPayable           = acct("2100", "Sales tax payable", AccountType.TaxPayable)
    val OwnersEquity         = acct("3000", "Owner's equity", AccountType.Equity)
    val OpeningBalanceEquity = acct("3900", "Opening balance equity", AccountType.Equity)
    val SubscriptionRevenue  = acct("4000", "Subscription revenue", AccountType.Income)
    val CourseRevenue        = acct("4100", "Course revenue", AccountType.Income)
    val Sales                = acct("4200", "Sales", AccountType.Income)
    val ProcessingFees       = acct("5000", "Payment processing fees", AccountType.CostOfSales)
    val InventoryAdjustments = acct("5100", "Inventory adjustments", AccountType.CostOfSales)
    val CostOfGoodsSold      = acct("5200", "Cost of goods sold", AccountType.CostOfSales)
    val GeneralExpenses      = acct("6000", "General & administrative", AccountType.Expense)
    val ExchangeGainLoss     = acct("7000", "Exchange gain or loss", AccountType.OtherExpense)

    val all: List[SystemAccount] = List(
      Cash,
      PaymentClearing,
      Receivables,
      InputTax,
      Inventory,
      Payables,
      TaxPayable,
      OwnersEquity,
      OpeningBalanceEquity,
      SubscriptionRevenue,
      CourseRevenue,
      Sales,
      ProcessingFees,
      InventoryAdjustments,
      CostOfGoodsSold,
      GeneralExpenses,
      ExchangeGainLoss
    )
  }

  def make[F[_]: Async](
      db: Db[F],
      posting: Posting[F],
      ledgerAccounts: LedgerAccounts[F],
      journals: Journals[F],
      exchangeRates: ExchangeRates[F]
  ): LedgerService[F] = new LedgerService[F] {

    private val base = posting.baseCurrency

    def bootstrap: F[Unit] =
      Async[F].delay(Instant.now()).flatMap { now =>
        SystemAccounts.all.traverse_(s =>
          ledgerAccounts.ensure(Account(s.id, s.code, s.name, s.accountType, base, None, active = true, now))
        )
      }

    def accounts: F[List[Account]] = ledgerAccounts.list()

    def createAccount(b: NewAccount): F[Either[LedgerError, Account]] =
      ledgerAccounts.list().flatMap { existing =>
        val parent = b.parentId.traverse(pid => existing.find(_.id == pid).toRight(LedgerError.UnknownParent(pid)))
        val checked: Either[LedgerError, Unit] = for {
          _ <- Either.cond(!existing.exists(_.code == b.code), (), LedgerError.DuplicateAccountCode(b.code))
          p <- parent
          _ <- p.filter(_.accountType.root != b.accountType.root)
            .map(pa => LedgerError.ParentTypeMismatch(pa.accountType.root, b.accountType.root))
            .toLeft(())
        } yield ()

        checked.traverse { _ =>
          for {
            id  <- Async[F].delay(AccountId.assume(UUID.randomUUID()))
            now <- Async[F].delay(Instant.now())
            a   <- ledgerAccounts.create(Account(
              id,
              b.code,
              b.name,
              b.accountType,
              b.currency.getOrElse(base),
              b.parentId,
              active = true,
              now
            ))
          } yield a
        }
      }

    def setActive(id: AccountId, active: Boolean): F[Option[Account]] =
      ledgerAccounts.setActive(id, active)

    def post(draft: Draft, by: UserId): F[Either[LedgerError, JournalEntry]] =
      (posting.chart, posting.rate(draft.currency, draft.exchangeRate, draft.date)).tupled.flatMap {
        case (_, Left(err))       => Async[F].pure(Left(err))
        case (chart, Right(rate)) =>
          posting.header(draft.date, draft.reference, draft.memo, draft.currency, rate, JournalSource.Manual, None, Some(by))
            .flatMap(h => db.txE(s => posting.postIn(s, h, draft.lines, chart)))
      }

    def reverse(id: JournalEntryId, date: LocalDate, by: UserId): F[Either[LedgerError, JournalEntry]] =
      posting.chart.flatMap(chart => db.txE(s => posting.reverseIn(s, id, date, Some(by), chart)))

    def postAutomated(draft: Draft, source: JournalSource, sourceId: UUID): F[Either[LedgerError, JournalEntry]] =
      (posting.chart, posting.rate(draft.currency, draft.exchangeRate, draft.date)).tupled.flatMap {
        case (_, Left(err))       => Async[F].pure(Left(err))
        case (chart, Right(rate)) =>
          posting.header(draft.date, draft.reference, draft.memo, draft.currency, rate, source, Some(sourceId), None).flatMap { h =>
            Ledger.entry(h, draft.lines, base, chart) match {
              case Left(err)    => Async[F].pure(Left(err))
              case Right(entry) =>
                journals.post(entry).flatMap {
                  case Journals.Outcome.Posted(e)           => Async[F].pure(Right(e))
                  case Journals.Outcome.Locked(l)           => Async[F].pure(Left(LedgerError.PeriodLocked(draft.date, l.lockThrough)))
                  case Journals.Outcome.Duplicate(existing) => journals.find(existing).map(_.toRight(LedgerError.EntryNotFound(existing)))
                }
            }
          }
      }

    def entry(id: JournalEntryId): F[Option[JournalEntry]] = journals.find(id)

    def entries(from: LocalDate, to: LocalDate, limit: Int): F[List[JournalEntry]] =
      journals.list(from, to, limit)

    def setRate(r: FxRate): F[Unit] = exchangeRates.upsert(r)

    def rates(currency: CurrencyCode): F[List[FxRate]] = exchangeRates.history(currency, 90)

    def lock: F[Option[LedgerLock]]     = journals.lock()
    def setLock(l: LedgerLock): F[Unit] = journals.setLock(l)
    def clearLock: F[Unit]              = journals.clearLock()

    def trialBalance(asOf: LocalDate): F[FinancialReports.TrialBalance] =
      (ledgerAccounts.list(), journals.totalsThrough(asOf))
        .mapN(FinancialReports.trialBalance(asOf, base, _, _))

    def profitAndLoss(from: LocalDate, to: LocalDate): F[Either[LedgerError, FinancialReports.ProfitAndLoss]] =
      period(from, to).traverse(_ =>
        (ledgerAccounts.list(), journals.totalsBetween(from, to))
          .mapN(FinancialReports.profitAndLoss(from, to, base, _, _))
      )

    def balanceSheet(asOf: LocalDate): F[FinancialReports.BalanceSheet] =
      (ledgerAccounts.list(), journals.totalsThrough(asOf))
        .mapN(FinancialReports.balanceSheet(asOf, base, _, _))

    def generalLedger(from: LocalDate, to: LocalDate, account: Option[AccountId]): F[Either[LedgerError, FinancialReports.GeneralLedger]] =
      period(from, to).traverse(_ =>
        (ledgerAccounts.list(), journals.totalsThrough(from.minusDays(1)), journals.rows(from, to)).mapN {
          (accts, opening, rows) =>
            val scoped = account.fold(accts)(id => accts.filter(_.id == id))
            FinancialReports.generalLedger(from, to, base, scoped, opening, rows)
        }
      )

    def revenueByUser(from: LocalDate, to: LocalDate): F[List[(UserId, Long)]] =
      journals.revenueByUser(from, to)
  }

  private[service] def period(from: LocalDate, to: LocalDate): Either[LedgerError, Unit] =
    Either.cond(!from.isAfter(to), (), LedgerError.InvalidPeriod(from, to))
}
