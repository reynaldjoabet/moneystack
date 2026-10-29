package moneystack.budget.db

import moneystack.domain.*
import moneystack.budget.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.{Command, Query, Session}
import skunk.implicits.*

/** Budgets (with category limits), schedules, objectives and automation rules, per owner. */
trait Plans[F[_]] {
  def createBudget(b: Budget): F[Budget]
  def budgets(owner: UserId): F[List[Budget]]
  def deleteBudget(owner: UserId, id: BudgetId): F[Boolean]

  def createSchedule(s: Schedule): F[Schedule]
  def schedules(owner: UserId): F[List[Schedule]]
  /** Moves a schedule on to occurrence `next` inside the caller's transaction, if it is still at `expected`. */
  def advanceIn(s: Session[F], id: ScheduleId, expected: Int, next: Int): F[Boolean]
  def setScheduleActive(owner: UserId, id: ScheduleId, active: Boolean): F[Option[Schedule]]

  def createObjective(o: Objective): F[Objective]
  def objectives(owner: UserId): F[List[Objective]]
  def setObjectiveArchived(owner: UserId, id: ObjectiveId, archived: Boolean): F[Option[Objective]]

  def createRule(r: Rule): F[Rule]
  def rules(owner: UserId): F[List[Rule]]
  def deleteRule(owner: UserId, id: RuleId): F[Boolean]
  def recordMatches(id: RuleId, count: Long): F[Unit]
}

object Plans {

  import Codecs.{budget as budgetC, budgetId as budgetIdC, limit as limitC, objective as objectiveC, objectiveId as objectiveIdC,
                 rule as ruleC, ruleId as ruleIdC, schedule as scheduleC, scheduleId as scheduleIdC, userId as userIdC}
  import skunk.codec.all.{bool, int4, int8}

  private def deleted(c: skunk.data.Completion) = c match {
    case skunk.data.Completion.Delete(n) => n > 0
    case _                               => false
  }

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Plans[F] = new Plans[F] {

    def createBudget(b: Budget): F[Budget] =
      pool.use(s => s.transaction.use { _ =>
        s.prepare(Q.insertBudget).flatMap(_.execute(b)) *>
          s.prepare(Q.insertLimit).flatMap(pc => b.categoryLimits.traverse_(l => pc.execute((b.id, l)))).as(b)
      })

    def budgets(owner: UserId): F[List[Budget]] =
      pool.use { s =>
        for {
          bs <- s.prepare(Q.budgets).flatMap(_.stream(owner, 64).compile.toList)
          ls <- s.prepare(Q.limits).flatMap(_.stream(owner, 256).compile.toList)
        } yield bs.map(b => b.copy(categoryLimits = ls.filter(_._1 == b.id).map(_._2)))
      }

    def deleteBudget(owner: UserId, id: BudgetId): F[Boolean] = pool.use(_.prepare(Q.deleteBudget).flatMap(_.execute((id, owner)))).map(deleted)

    def createSchedule(s: Schedule): F[Schedule] = pool.use(_.prepare(Q.insertSchedule).flatMap(_.unique(s)))

    def schedules(owner: UserId): F[List[Schedule]] = pool.use(_.prepare(Q.schedules).flatMap(_.stream(owner, 64).compile.toList))

    def advanceIn(s: Session[F], id: ScheduleId, expected: Int, next: Int): F[Boolean] =
      s.prepare(Q.advance).flatMap(_.execute((next, id, expected))).map {
        case skunk.data.Completion.Update(n) => n > 0
        case _                               => false
      }

    def setScheduleActive(owner: UserId, id: ScheduleId, active: Boolean): F[Option[Schedule]] =
      pool.use(_.prepare(Q.setScheduleActive).flatMap(_.option((active, id, owner))))

    def createObjective(o: Objective): F[Objective] = pool.use(_.prepare(Q.insertObjective).flatMap(_.unique(o)))

    def objectives(owner: UserId): F[List[Objective]] = pool.use(_.prepare(Q.objectives).flatMap(_.stream(owner, 64).compile.toList))

    def setObjectiveArchived(owner: UserId, id: ObjectiveId, archived: Boolean): F[Option[Objective]] =
      pool.use(_.prepare(Q.archiveObjective).flatMap(_.option((archived, id, owner))))

    def createRule(r: Rule): F[Rule] = pool.use(_.prepare(Q.insertRule).flatMap(_.unique(r)))

    def rules(owner: UserId): F[List[Rule]] = pool.use(_.prepare(Q.rules).flatMap(_.stream(owner, 128).compile.toList))

    def deleteRule(owner: UserId, id: RuleId): F[Boolean] = pool.use(_.prepare(Q.deleteRule).flatMap(_.execute((id, owner)))).map(deleted)

    def recordMatches(id: RuleId, count: Long): F[Unit] =
      if (count <= 0) Concurrent[F].unit else pool.use(_.prepare(Q.recordMatches).flatMap(_.execute((count, id)))).void
  }

  private object Q {

    private val budgetCols =
      sql"""id, owner_id, name, amount_minor, currency, recurrence, period_length, start_date, end_date, wallet_ids, category_ids,
            excluded_category_ids, income, rollover, pinned, archived, created_at"""

    val insertBudget: Command[Budget] = sql"INSERT INTO budget.budgets ($budgetCols) VALUES ${budgetC.values}".command

    val insertLimit: Command[(BudgetId, CategoryLimit)] =
      sql"INSERT INTO budget.category_limits (budget_id, category_id, amount_minor) VALUES ($budgetIdC, $limitC)".command

    val budgets: Query[UserId, Budget] =
      sql"SELECT $budgetCols FROM budget.budgets WHERE owner_id = $userIdC ORDER BY pinned DESC, name".query(budgetC)

    val limits: Query[UserId, (BudgetId, CategoryLimit)] =
      sql"""SELECT l.budget_id, l.category_id, l.amount_minor FROM budget.category_limits l
            JOIN budget.budgets b ON b.id = l.budget_id WHERE b.owner_id = $userIdC""".query(budgetIdC *: limitC)

    val deleteBudget: Command[(BudgetId, UserId)] =
      sql"DELETE FROM budget.budgets WHERE id = $budgetIdC AND owner_id = $userIdC".command

    private val scheduleCols =
      sql"""id, owner_id, kind, title, wallet_id, tx_kind, amount_minor, category_id, payee_id, note, frequency, every,
            start_date, end_date, max_occurrences, auto_pay, remind_days_before, next_index, active, created_at"""

    val insertSchedule: Query[Schedule, Schedule] =
      sql"INSERT INTO budget.schedules ($scheduleCols) VALUES ${scheduleC.values} RETURNING $scheduleCols".query(scheduleC)

    val schedules: Query[UserId, Schedule] =
      sql"SELECT $scheduleCols FROM budget.schedules WHERE owner_id = $userIdC ORDER BY title".query(scheduleC)

    val advance: Command[(Int, ScheduleId, Int)] =
      sql"UPDATE budget.schedules SET next_index = $int4 WHERE id = $scheduleIdC AND next_index = $int4".command

    val setScheduleActive: Query[(Boolean, ScheduleId, UserId), Schedule] =
      sql"""UPDATE budget.schedules SET active = $bool WHERE id = $scheduleIdC AND owner_id = $userIdC
            RETURNING $scheduleCols""".query(scheduleC)

    private val objectiveCols = sql"id, owner_id, kind, name, target_minor, currency, due_date, counterparty, archived, created_at"

    val insertObjective: Query[Objective, Objective] =
      sql"INSERT INTO budget.objectives ($objectiveCols) VALUES ${objectiveC.values} RETURNING $objectiveCols".query(objectiveC)

    val objectives: Query[UserId, Objective] =
      sql"SELECT $objectiveCols FROM budget.objectives WHERE owner_id = $userIdC ORDER BY archived, name".query(objectiveC)

    val archiveObjective: Query[(Boolean, ObjectiveId, UserId), Objective] =
      sql"""UPDATE budget.objectives SET archived = $bool WHERE id = $objectiveIdC AND owner_id = $userIdC
            RETURNING $objectiveCols""".query(objectiveC)

    private val ruleCols = sql"id, owner_id, name, position, enabled, match_all, conditions, actions, match_count, created_at"

    val insertRule: Query[Rule, Rule] =
      sql"INSERT INTO budget.rules ($ruleCols) VALUES ${ruleC.values} RETURNING $ruleCols".query(ruleC)

    val rules: Query[UserId, Rule] =
      sql"SELECT $ruleCols FROM budget.rules WHERE owner_id = $userIdC ORDER BY position, created_at".query(ruleC)

    val deleteRule: Command[(RuleId, UserId)] =
      sql"DELETE FROM budget.rules WHERE id = $ruleIdC AND owner_id = $userIdC".command

    val recordMatches: Command[(Long, RuleId)] =
      sql"UPDATE budget.rules SET match_count = match_count + $int8 WHERE id = $ruleIdC".command
  }
}
