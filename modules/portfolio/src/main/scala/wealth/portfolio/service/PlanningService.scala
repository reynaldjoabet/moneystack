package wealth.portfolio.service

import wealth.domain.*
import wealth.portfolio.domain.*
import wealth.portfolio.db.{Accounts, Plans}

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/** Savings goals funded by account shares, save-up projections and FIRE planning. */
trait PlanningService[F[_]] {
  def createGoal(owner: UserId, b: PlanningService.NewGoal): F[Either[PortfolioError, Goal]]
  def deleteGoal(owner: UserId, id: GoalId): F[Either[PortfolioError, Unit]]
  /** Each goal's funded value on `asOf` and its projection. */
  def goals(owner: UserId, asOf: LocalDate): F[Either[PortfolioError, List[PlanningService.GoalProgress]]]
  def saveUp(in: SaveUpInput, asOf: LocalDate): Either[PortfolioError, SaveUpOverview]
  def fire(in: FireInput): Either[PortfolioError, FireResult]
}

object PlanningService {

  final case class NewGoal(
      name: Title,
      targetAmount: BigDecimal,
      currency: CurrencyCode,
      targetDate: Option[LocalDate],
      monthlyContribution: Option[BigDecimal],
      expectedAnnualReturn: Option[BigDecimal],
      funding: List[GoalFunding]
  )

  final case class GoalProgress(goal: Goal, currentValue: BigDecimal, overview: SaveUpOverview)

  def make[F[_]: Async](portfolio: PortfolioService[F], accounts: Accounts[F], plans: Plans[F]): PlanningService[F] =
    new PlanningService[F] {

      def createGoal(owner: UserId, b: NewGoal): F[Either[PortfolioError, Goal]] =
        accounts.list(owner).flatMap { owned =>
          val checked = for {
            _ <- Either.cond(b.targetAmount > 0, (), PortfolioError.InvalidInput("the target must be positive"))
            _ <- Either.cond(b.monthlyContribution.forall(_ >= 0), (), PortfolioError.InvalidInput("the monthly contribution must not be negative"))
            _ <- Either.cond(b.expectedAnnualReturn.forall(r => r >= -0.2 && r <= 0.5), (), PortfolioError.InvalidInput("expected return must be between -20% and 50%"))
            _ <- Either.cond(b.funding.map(_.accountId).distinct.size == b.funding.size, (), PortfolioError.InvalidInput("each account may fund a goal once"))
            _ <- b.funding.traverse_(f => owned.find(_.id == f.accountId).toRight(PortfolioError.AccountNotFound(f.accountId)))
          } yield ()
          checked.traverse { _ =>
            for {
              id  <- Async[F].delay(GoalId.assume(UUID.randomUUID()))
              now <- Async[F].delay(Instant.now())
              g   <- plans.createGoal(Goal(id, owner, b.name, b.targetAmount, b.currency, b.targetDate,
                                          b.monthlyContribution.getOrElse(BigDecimal(0)), b.expectedAnnualReturn.getOrElse(BigDecimal(0)),
                                          b.funding, now))
            } yield g
          }
        }

      def deleteGoal(owner: UserId, id: GoalId): F[Either[PortfolioError, Unit]] =
        plans.deleteGoal(owner, id).map(found => Either.cond(found, (), PortfolioError.GoalNotFound(id)))

      def goals(owner: UserId, asOf: LocalDate): F[Either[PortfolioError, List[GoalProgress]]] =
        plans.goals(owner).flatMap { gs =>
          gs.traverse { g =>
            (for {
              values <- EitherT(portfolio.accountValues(owner, asOf, g.currency))
            } yield {
              val current = g.funding.map(f => values.getOrElse(f.accountId, BigDecimal(0)) * f.shareBps.value / 10_000).sum
                .setScale(2, BigDecimal.RoundingMode.HALF_EVEN)
              GoalProgress(g, current, SaveUp.overview(SaveUpInput(current, g.targetAmount, g.targetDate, g.monthlyContribution, g.expectedAnnualReturn), asOf))
            }).value
          }.map(_.sequence)
        }

      def saveUp(in: SaveUpInput, asOf: LocalDate): Either[PortfolioError, SaveUpOverview] =
        for {
          _ <- Either.cond(in.targetAmount > 0 && in.currentValue >= 0 && in.monthlyContribution >= 0, (),
                           PortfolioError.InvalidInput("amounts must not be negative and the target must be positive"))
          _ <- Either.cond(in.expectedAnnualReturn >= -0.2 && in.expectedAnnualReturn <= 0.5, (),
                           PortfolioError.InvalidInput("expected return must be between -20% and 50%"))
        } yield SaveUp.overview(in, asOf)

      def fire(in: FireInput): Either[PortfolioError, FireResult] =
        Fire.validate(in).map(_ => Fire.run(in))

    }
}
