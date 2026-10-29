package moneystack.portfolio.db

import moneystack.domain.*
import moneystack.portfolio.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Allocation targets, contribution limits and goals, per owner. */
trait Plans[F[_]] {
  /** Replaces the owner's targets under one taxonomy. */
  def setTargets(owner: UserId, taxonomy: Taxonomy, ts: List[AllocationTarget]): F[Unit]
  def targets(owner: UserId, taxonomy: Taxonomy): F[List[AllocationTarget]]

  def createLimit(l: ContributionLimit): F[ContributionLimit]
  def limits(owner: UserId): F[List[ContributionLimit]]
  def deleteLimit(owner: UserId, id: LimitId): F[Boolean]

  def createGoal(g: Goal): F[Goal]
  def goals(owner: UserId): F[List[Goal]]
  def deleteGoal(owner: UserId, id: GoalId): F[Boolean]
}

object Plans {

  import Codecs.{accountId as accountIdC, goal as goalC, goalId as goalIdC, limit as limitC, limitId as limitIdC,
                 target as targetC, taxonomy as taxonomyC, userId as userIdC}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Plans[F] = new Plans[F] {

    private def deleted(c: skunk.data.Completion) = c match {
      case skunk.data.Completion.Delete(n) => n > 0
      case _                               => false
    }

    def setTargets(owner: UserId, taxonomy: Taxonomy, ts: List[AllocationTarget]): F[Unit] =
      pool.use { s =>
        s.transaction.use { _ =>
          s.prepare(Q.clearTargets).flatMap(_.execute((owner, taxonomy))) *>
            s.prepare(Q.insertTarget).flatMap(pc => ts.traverse_(pc.execute))
        }
      }

    def targets(owner: UserId, taxonomy: Taxonomy): F[List[AllocationTarget]] =
      pool.use(_.prepare(Q.targets).flatMap(_.stream((owner, taxonomy), 64).compile.toList))

    def createLimit(l: ContributionLimit): F[ContributionLimit] =
      pool.use { s =>
        s.transaction.use { _ =>
          s.prepare(Q.insertLimit).flatMap(_.execute(l)) *>
            s.prepare(Q.insertLimitAccount).flatMap(pc => l.accountIds.traverse_(a => pc.execute((l.id, a)))).as(l)
        }
      }

    def limits(owner: UserId): F[List[ContributionLimit]] =
      pool.use { s =>
        for {
          ls   <- s.prepare(Q.limits).flatMap(_.stream(owner, 64).compile.toList)
          accs <- s.prepare(Q.limitAccounts).flatMap(_.stream(owner, 256).compile.toList)
        } yield ls.map(l => l.copy(accountIds = accs.filter(_._1 == l.id).map(_._2)))
      }

    def deleteLimit(owner: UserId, id: LimitId): F[Boolean] =
      pool.use(_.prepare(Q.deleteLimit).flatMap(_.execute((id, owner)))).map(deleted)

    def createGoal(g: Goal): F[Goal] =
      pool.use { s =>
        s.transaction.use { _ =>
          s.prepare(Q.insertGoal).flatMap(_.execute(g)) *>
            s.prepare(Q.insertFunding).flatMap(pc => g.funding.traverse_(f => pc.execute((g.id, f.accountId, f.shareBps)))).as(g)
        }
      }

    def goals(owner: UserId): F[List[Goal]] =
      pool.use { s =>
        for {
          gs <- s.prepare(Q.goals).flatMap(_.stream(owner, 64).compile.toList)
          fs <- s.prepare(Q.funding).flatMap(_.stream(owner, 256).compile.toList)
        } yield gs.map(g => g.copy(funding = fs.filter(_._1 == g.id).map(f => GoalFunding(f._2, f._3))))
      }

    def deleteGoal(owner: UserId, id: GoalId): F[Boolean] =
      pool.use(_.prepare(Q.deleteGoal).flatMap(_.execute((id, owner)))).map(deleted)
  }

  private object Q {

    val clearTargets: Command[(UserId, Taxonomy)] =
      sql"DELETE FROM portfolio.allocation_targets WHERE owner_id = $userIdC AND taxonomy = $taxonomyC".command

    val insertTarget: Command[AllocationTarget] =
      sql"INSERT INTO portfolio.allocation_targets (id, owner_id, taxonomy, category, weight_bps) VALUES ${targetC.values}".command

    val targets: Query[(UserId, Taxonomy), AllocationTarget] =
      sql"""SELECT id, owner_id, taxonomy, category, weight_bps FROM portfolio.allocation_targets
            WHERE owner_id = $userIdC AND taxonomy = $taxonomyC ORDER BY category""".query(targetC)

    private val limitCols = sql"id, owner_id, name, contribution_year, limit_amount, currency, start_date, end_date"

    val insertLimit: Command[ContributionLimit] =
      sql"INSERT INTO portfolio.contribution_limits ($limitCols) VALUES ${limitC.values}".command

    val insertLimitAccount: Command[(LimitId, InvestmentAccountId)] =
      sql"INSERT INTO portfolio.contribution_limit_accounts (limit_id, account_id) VALUES ($limitIdC, $accountIdC)".command

    val limits: Query[UserId, ContributionLimit] =
      sql"SELECT $limitCols FROM portfolio.contribution_limits WHERE owner_id = $userIdC ORDER BY contribution_year DESC, name".query(limitC)

    val limitAccounts: Query[UserId, (LimitId, InvestmentAccountId)] =
      sql"""SELECT la.limit_id, la.account_id FROM portfolio.contribution_limit_accounts la
            JOIN portfolio.contribution_limits l ON l.id = la.limit_id WHERE l.owner_id = $userIdC""".query(limitIdC *: accountIdC)

    val deleteLimit: Command[(LimitId, UserId)] =
      sql"DELETE FROM portfolio.contribution_limits WHERE id = $limitIdC AND owner_id = $userIdC".command

    private val goalCols =
      sql"id, owner_id, name, target_amount, currency, target_date, monthly_contribution, expected_return, created_at"

    val insertGoal: Command[Goal] = sql"INSERT INTO portfolio.goals ($goalCols) VALUES ${goalC.values}".command

    val insertFunding: Command[(GoalId, InvestmentAccountId, Bps)] =
      sql"INSERT INTO portfolio.goal_funding (goal_id, account_id, share_bps) VALUES ($goalIdC, $accountIdC, ${Codecs.bps})".command

    val goals: Query[UserId, Goal] =
      sql"SELECT $goalCols FROM portfolio.goals WHERE owner_id = $userIdC ORDER BY created_at".query(goalC)

    val funding: Query[UserId, (GoalId, InvestmentAccountId, Bps)] =
      sql"""SELECT f.goal_id, f.account_id, f.share_bps FROM portfolio.goal_funding f
            JOIN portfolio.goals g ON g.id = f.goal_id WHERE g.owner_id = $userIdC""".query(goalIdC *: accountIdC *: Codecs.bps)

    val deleteGoal: Command[(GoalId, UserId)] =
      sql"DELETE FROM portfolio.goals WHERE id = $goalIdC AND owner_id = $userIdC".command

  }
}
