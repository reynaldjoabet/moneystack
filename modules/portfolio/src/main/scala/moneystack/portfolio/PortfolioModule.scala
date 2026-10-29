package moneystack.portfolio

import moneystack.domain.*
import moneystack.portfolio.db.*
import moneystack.portfolio.http.PortfolioRoutes
import moneystack.portfolio.service.*

import cats.effect.*
import org.http4s.{HttpRoutes, Request}
import skunk.Session

/** The portfolio module's services, wired over one session pool. */
final class PortfolioModule[F[_]: Async] private (
    defaultCurrency: CurrencyCode,
    val portfolio: PortfolioService[F],
    val planning: PlanningService[F],
    val netWorth: NetWorthService[F]
) {

  /** Routes under `/portfolio`, acting as the user `auth` returns. */
  def routes(auth: Request[F] => F[Option[UserId]]): HttpRoutes[F] =
    new PortfolioRoutes[F](auth, defaultCurrency, portfolio, planning, netWorth).routes
}

object PortfolioModule {

  /** `defaultCurrency` is the reporting currency when a request doesn't name one. */
  def make[F[_]: Async](pool: Resource[F, Session[F]], defaultCurrency: CurrencyCode): PortfolioModule[F] = {
    val accounts   = Accounts.make[F](pool)
    val market     = Market.make[F](pool)
    val activities = Activities.make[F](pool)
    val plans      = Plans.make[F](pool)
    val wealth     = Wealth.make[F](pool)
    val portfolio  = PortfolioService.make[F](accounts, market, activities, plans)
    new PortfolioModule[F](
      defaultCurrency,
      portfolio,
      PlanningService.make[F](portfolio, accounts, plans),
      NetWorthService.make[F](portfolio, accounts, market, wealth)
    )
  }
}
