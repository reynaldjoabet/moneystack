package wealth.accounting

import wealth.domain.*
import wealth.accounting.db.*
import wealth.accounting.http.{BankingRoutes, LedgerRoutes, TradeRoutes}
import wealth.accounting.service.*
import wealth.platform.Db

import cats.effect.*
import cats.syntax.all.*
import org.http4s.{HttpRoutes, Request}
import skunk.Session

/** The accounting module's services, wired over one session pool. */
final class AccountingModule[F[_]: Async] private (
    val ledger: LedgerService[F],
    val catalog: CatalogService[F],
    val trade: TradeService[F],
    val banking: BankingService[F]
) {

  /** Creates the system chart of accounts. Run once at startup. */
  def bootstrap: F[Unit] = ledger.bootstrap

  /** All accountant routes; `auth` returns the caller's user id when they may use them. */
  def routes(auth: Request[F] => F[Option[UserId]]): HttpRoutes[F] =
    new LedgerRoutes[F](auth, ledger).routes <+>
      new TradeRoutes[F](auth, catalog, trade).routes <+>
      new BankingRoutes[F](auth, banking).routes
}

object AccountingModule {

  def make[F[_]: Async](pool: Resource[F, Session[F]], baseCurrency: CurrencyCode): AccountingModule[F] = {
    val db          = new Db[F](pool)
    val accounts    = LedgerAccounts.make[F](pool)
    val journals    = Journals.make[F](pool)
    val rates       = ExchangeRates.make[F](pool)
    val contacts    = Contacts.make[F](pool)
    val taxRates    = TaxRates.make[F](pool)
    val items       = Items.make[F](pool)
    val documents   = Documents.make[F](pool)
    val settlements = Settlements.make[F](pool)
    val banking     = Banking.make[F](pool)
    val posting     = new Posting[F](baseCurrency, accounts, journals, rates)

    new AccountingModule[F](
      LedgerService.make[F](db, posting, accounts, journals, rates),
      CatalogService.make[F](posting, contacts, taxRates, items),
      TradeService.make[F](db, posting, journals, contacts, items, taxRates, documents, settlements),
      BankingService.make[F](db, posting, banking, contacts)
    )
  }
}
