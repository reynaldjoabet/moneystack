package wealth.budget

import wealth.domain.*
import wealth.budget.db.*
import wealth.budget.http.BudgetRoutes
import wealth.budget.service.*
import wealth.platform.Db

import cats.effect.*
import org.http4s.{HttpRoutes, Request}
import skunk.Session

/** The budget module's services, wired over one session pool. */
final class BudgetModule[F[_]: Async] private (
    defaultCurrency: CurrencyCode,
    val money: MoneyService[F],
    val plans: PlanService[F],
    val insights: InsightService[F]
) {

  /** Routes under `/budget`, acting as the user `auth` returns. */
  def routes(auth: Request[F] => F[Option[UserId]]): HttpRoutes[F] =
    new BudgetRoutes[F](auth, defaultCurrency, money, plans, insights).routes
}

object BudgetModule {

  def make[F[_]: Async](pool: Resource[F, Session[F]], defaultCurrency: CurrencyCode): BudgetModule[F] = {
    val wallets      = Wallets.make[F](pool)
    val catalog      = Catalog.make[F](pool)
    val transactions = Transactions.make[F](pool)
    val plans        = Plans.make[F](pool)
    val loader       = new Loader[F](wallets, catalog, transactions)
    new BudgetModule[F](
      defaultCurrency,
      MoneyService.make[F](wallets, catalog, transactions, plans),
      PlanService.make[F](new Db[F](pool), loader, wallets, catalog, transactions, plans),
      InsightService.make[F](loader)
    )
  }
}
