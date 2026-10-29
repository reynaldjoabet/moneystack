package moneystack.budget.service

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.budget.db.{Catalog, Plans, Transactions, Wallets}
import moneystack.platform.Db

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Looking ahead: budgets with period status, schedules (subscriptions,
 * bills, instalments, repeating items) with upcoming occurrences and
 * payment, and objectives (savings goals, money lent or borrowed).
 */
trait PlanService[F[_]] {
  def createBudget(owner: UserId, b: PlanService.NewBudget): F[Either[BudgetError, Budget]]
  def deleteBudget(owner: UserId, id: BudgetId): F[Either[BudgetError, Unit]]
  /** Every unarchived budget with its current period; budgets not running on `asOf` have no status. */
  def budgets(owner: UserId, asOf: LocalDate): F[Either[BudgetError, List[PlanService.BudgetView]]]
  def budgetHistory(owner: UserId, id: BudgetId, asOf: LocalDate, periods: Int): F[Either[BudgetError, List[PlanService.PeriodSpend]]]

  def createSchedule(owner: UserId, b: PlanService.NewSchedule): F[Either[BudgetError, Schedule]]
  def setScheduleActive(owner: UserId, id: ScheduleId, active: Boolean): F[Option[Schedule]]
  def schedules(owner: UserId): F[List[PlanService.ScheduleView]]
  /** Unpaid occurrences up to `days` ahead, plus anything overdue. */
  def upcoming(owner: UserId, today: LocalDate, days: Int): F[List[PlanService.Upcoming]]
  /** Records the next occurrence as a transaction (optionally on another date or for another amount). */
  def pay(owner: UserId, id: ScheduleId, on: Option[LocalDate], amountMinor: Option[Long]): F[Either[BudgetError, Transaction]]
  def skip(owner: UserId, id: ScheduleId): F[Either[BudgetError, Option[LocalDate]]]
  /** Records every auto-pay occurrence due on or before `asOf`; safe to run repeatedly. */
  def processDue(owner: UserId, asOf: LocalDate): F[Int]
  def subscriptionCost(owner: UserId, currency: CurrencyCode, asOf: LocalDate): F[Either[BudgetError, PlanService.SubscriptionCost]]

  def createObjective(owner: UserId, b: PlanService.NewObjective): F[Either[BudgetError, Objective]]
  def setObjectiveArchived(owner: UserId, id: ObjectiveId, archived: Boolean): F[Option[Objective]]
  def objectives(owner: UserId): F[Either[BudgetError, List[ObjectiveProgress]]]
}

object PlanService {

  final case class NewBudget(
      name: Title,
      amountMinor: Long,
      currency: CurrencyCode,
      recurrence: Recurrence,
      periodLength: Option[Int],
      startDate: LocalDate,
      endDate: Option[LocalDate],
      walletIds: Option[List[WalletId]],
      categoryIds: Option[List[CategoryId]],
      excludedCategoryIds: Option[List[CategoryId]],
      income: Option[Boolean],
      rollover: Option[Boolean],
      categoryLimits: Option[List[CategoryLimit]],
      pinned: Option[Boolean]
  )

  final case class BudgetView(budget: Budget, status: Option[BudgetStatus])

  final case class PeriodSpend(period: Period, spentMinor: Long, amountMinor: Long)

  final case class NewSchedule(
      kind: ScheduleKind,
      title: Title,
      walletId: WalletId,
      txKind: TxKind,
      amountMinor: Long,
      categoryId: Option[CategoryId],
      payeeId: Option[PayeeId],
      note: Option[Body],
      frequency: Frequency,
      interval: Option[Int],
      startDate: LocalDate,
      endDate: Option[LocalDate],
      maxOccurrences: Option[Int],
      autoPay: Option[Boolean],
      remindDaysBefore: Option[Int]
  )

  final case class ScheduleView(schedule: Schedule, nextDue: Option[LocalDate], monthlyCostMinor: Long)

  final case class Upcoming(scheduleId: ScheduleId, title: Title, kind: ScheduleKind, txKind: TxKind, due: LocalDate,
                            amountMinor: Long, currency: CurrencyCode, overdue: Boolean, remind: Boolean)

  final case class SubscriptionCost(currency: CurrencyCode, monthlyMinor: Long, yearlyMinor: Long, count: Int)

  final case class NewObjective(kind: ObjectiveKind, name: Title, targetMinor: Long, currency: CurrencyCode,
                                dueDate: Option[LocalDate], counterparty: Option[Title])

  def make[F[_]: Async](db: Db[F], loader: Loader[F], wallets: Wallets[F], catalog: Catalog[F], txRepo: Transactions[F], plans: Plans[F]): PlanService[F] =
    new PlanService[F] {

      private type R[A] = EitherT[F, BudgetError, A]
      private def lift[A](fa: F[A]): R[A] = EitherT.liftF(fa)
      private def check(ok: Boolean, err: => BudgetError): R[Unit] = EitherT.cond[F](ok, (), err)
      private def uuid = Async[F].delay(UUID.randomUUID())

      // ---- budgets ----

      def createBudget(owner: UserId, b: NewBudget): F[Either[BudgetError, Budget]] =
        (for {
          ws   <- lift(wallets.owned(owner))
          cs   <- lift(catalog.categories(owner))
          len   = b.periodLength.getOrElse(1)
          _    <- check(b.amountMinor > 0, BudgetError.Invalid("the amount must be positive"))
          _    <- check(len >= 1 && len <= 366, BudgetError.Invalid("the period length must be 1-366"))
          _    <- check(b.recurrence != Recurrence.Custom || b.endDate.isDefined, BudgetError.Invalid("a custom budget needs an end date"))
          _    <- check(b.endDate.forall(!_.isBefore(b.startDate)), BudgetError.Invalid("the end date is before the start"))
          _    <- b.walletIds.getOrElse(Nil).traverse_(w => check(ws.exists(_.id == w), BudgetError.WalletNotFound(w)))
          cats  = b.categoryIds.getOrElse(Nil) ++ b.excludedCategoryIds.getOrElse(Nil) ++ b.categoryLimits.getOrElse(Nil).map(_.categoryId)
          _    <- cats.traverse_(c => check(cs.exists(_.id == c), BudgetError.CategoryNotFound(c)))
          _    <- check(b.categoryLimits.getOrElse(Nil).forall(_.amountMinor > 0), BudgetError.Invalid("category limits must be positive"))
          id   <- lift(uuid)
          now  <- lift(Async[F].delay(Instant.now()))
          out  <- lift(plans.createBudget(Budget(BudgetId.assume(id), owner, b.name, b.amountMinor, b.currency, b.recurrence, len, b.startDate,
                                                 b.endDate, b.walletIds.getOrElse(Nil).distinct, b.categoryIds.getOrElse(Nil).distinct,
                                                 b.excludedCategoryIds.getOrElse(Nil).distinct, b.income.getOrElse(false), b.rollover.getOrElse(false),
                                                 b.categoryLimits.getOrElse(Nil).distinctBy(_.categoryId), b.pinned.getOrElse(false), archived = false, now)))
        } yield out).value

      def deleteBudget(owner: UserId, id: BudgetId): F[Either[BudgetError, Unit]] =
        plans.deleteBudget(owner, id).map(found => Either.cond(found, (), BudgetError.BudgetNotFound(id)))

      def budgets(owner: UserId, asOf: LocalDate): F[Either[BudgetError, List[BudgetView]]] =
        (plans.budgets(owner), loader.load(owner)).tupled.map { case (bs, snap) =>
          bs.filterNot(_.archived).traverse { b =>
            snap.entries(b.currency).map(es => BudgetView(b, Reports.status(b, es, snap.categories, asOf)))
          }
        }

      def budgetHistory(owner: UserId, id: BudgetId, asOf: LocalDate, periods: Int): F[Either[BudgetError, List[PeriodSpend]]] =
        (plans.budgets(owner), loader.load(owner)).tupled.map { case (bs, snap) =>
          for {
            b  <- bs.find(_.id == id).toRight(BudgetError.BudgetNotFound(id))
            es <- snap.entries(b.currency)
          } yield Reports.history(b, es, snap.categories, asOf, periods.max(1).min(60)).map { case (p, s) => PeriodSpend(p, s, b.amountMinor) }
        }

      // ---- schedules ----

      def createSchedule(owner: UserId, b: NewSchedule): F[Either[BudgetError, Schedule]] =
        (for {
          ws  <- lift(wallets.owned(owner))
          cs  <- lift(catalog.categories(owner))
          ps  <- lift(catalog.payees(owner))
          _   <- check(ws.exists(_.id == b.walletId), BudgetError.WalletNotFound(b.walletId))
          _   <- check(b.txKind != TxKind.Transfer, BudgetError.Invalid("schedules record income or expenses"))
          _   <- check(b.amountMinor > 0, BudgetError.Invalid("the amount must be positive"))
          _   <- check(b.interval.forall(i => i >= 1 && i <= 366), BudgetError.Invalid("the interval must be 1-366"))
          _   <- check(b.maxOccurrences.forall(_ > 0), BudgetError.Invalid("max occurrences must be positive"))
          _   <- check(b.endDate.forall(!_.isBefore(b.startDate)), BudgetError.Invalid("the end date is before the start"))
          _   <- check(b.remindDaysBefore.forall(d => d >= 0 && d <= 60), BudgetError.Invalid("reminders are 0-60 days ahead"))
          _   <- b.categoryId.traverse_(c => check(cs.exists(x => x.id == c && (x.kind == CategoryKind.Income) == (b.txKind == TxKind.Income)),
                                                   BudgetError.CategoryNotFound(c)))
          _   <- b.payeeId.traverse_(p => check(ps.exists(_.id == p), BudgetError.PayeeNotFound(p)))
          id  <- lift(uuid)
          now <- lift(Async[F].delay(Instant.now()))
          out <- lift(plans.createSchedule(Schedule(ScheduleId.assume(id), owner, b.kind, b.title, b.walletId, b.txKind, b.amountMinor, b.categoryId,
                                                    b.payeeId, b.note, b.frequency, b.interval.getOrElse(1), b.startDate, b.endDate, b.maxOccurrences,
                                                    b.autoPay.getOrElse(false), b.remindDaysBefore.getOrElse(3), 0, active = true, now)))
        } yield out).value

      def setScheduleActive(owner: UserId, id: ScheduleId, active: Boolean): F[Option[Schedule]] = plans.setScheduleActive(owner, id, active)

      def schedules(owner: UserId): F[List[ScheduleView]] =
        plans.schedules(owner).map(_.map(s => ScheduleView(s, Periods.nextDue(s), Periods.monthlyCost(s).toLong)))

      def upcoming(owner: UserId, today: LocalDate, days: Int): F[List[Upcoming]] =
        (plans.schedules(owner), wallets.owned(owner)).tupled.map { case (ss, ws) =>
          val ccy = ws.map(w => w.id -> w.currency).toMap
          ss.flatMap { s =>
            Periods.occurrences(s, LocalDate.MIN, today.plusDays(days.toLong.max(0))).take(366).map { case (_, due) =>
              Upcoming(s.id, s.title, s.kind, s.txKind, due, s.amountMinor, ccy.getOrElse(s.walletId, CurrencyCode.USD), due.isBefore(today),
                       !due.isAfter(today.plusDays(s.remindDaysBefore.toLong)))
            }
          }.sortBy(_.due)
        }

      private def owned(owner: UserId, id: ScheduleId): R[Schedule] =
        EitherT(plans.schedules(owner).map(_.find(_.id == id).toRight(BudgetError.ScheduleNotFound(id))))

      /** Records occurrence `s.nextIndex` and moves the schedule on, atomically. */
      private def record(s: Schedule, on: Option[LocalDate], amount: Option[Long]): F[Either[BudgetError, Transaction]] =
        Periods.nextDue(s) match {
          case None => Async[F].pure(Left(BudgetError.ScheduleFinished(s.id)))
          case Some(due) =>
            for {
              id  <- uuid
              now <- Async[F].delay(Instant.now())
              t    = Transaction(TransactionId.assume(id), s.ownerId, s.walletId, s.txKind, amount.getOrElse(s.amountMinor), on.getOrElse(due),
                                 s.title, s.note, s.categoryId, if (s.categoryId.isDefined) CategorySource.Schedule else CategorySource.Unset,
                                 s.payeeId, Nil, Nil, None, None, None, None, Some(s.id), Some(due), Nil, None, s.ownerId, now)
              res <- db.txE[BudgetError, Transaction] { session =>
                       for {
                         saved <- EitherT.fromOptionF(txRepo.insertIfNewIn(session, t), BudgetError.AlreadyRecorded(s.id, due): BudgetError)
                         moved <- EitherT.liftF(plans.advanceIn(session, s.id, s.nextIndex, s.nextIndex + 1))
                         _     <- EitherT.cond[F](moved, (), BudgetError.AlreadyRecorded(s.id, due): BudgetError)
                       } yield saved
                     }
            } yield res
        }

      def pay(owner: UserId, id: ScheduleId, on: Option[LocalDate], amountMinor: Option[Long]): F[Either[BudgetError, Transaction]] =
        (for {
          s <- owned(owner, id)
          _ <- check(amountMinor.forall(_ > 0), BudgetError.Invalid("the amount must be positive"))
          t <- EitherT(record(s, on, amountMinor))
        } yield t).value

      def skip(owner: UserId, id: ScheduleId): F[Either[BudgetError, Option[LocalDate]]] =
        (for {
          s     <- owned(owner, id)
          _     <- EitherT.fromOption[F](Periods.nextDue(s), BudgetError.ScheduleFinished(id): BudgetError)
          moved <- lift(db.tx(session => plans.advanceIn(session, s.id, s.nextIndex, s.nextIndex + 1)))
          _     <- check(moved, BudgetError.Invalid("the schedule changed meanwhile; retry"))
        } yield Periods.nextDue(s.copy(nextIndex = s.nextIndex + 1))).value

      def processDue(owner: UserId, asOf: LocalDate): F[Int] =
        plans.schedules(owner).flatMap { ss =>
          ss.filter(s => s.active && s.autoPay).traverse { s =>
            // Pay one occurrence at a time from the latest state until nothing is due.
            def loop(cur: Schedule, n: Int): F[Int] =
              Periods.nextDue(cur).filter(!_.isAfter(asOf)) match {
                case None => Async[F].pure(n)
                case Some(_) =>
                  record(cur, None, None).flatMap {
                    case Right(_) => loop(cur.copy(nextIndex = cur.nextIndex + 1), n + 1)
                    case Left(_)  => Async[F].pure(n)
                  }
              }
            loop(s, 0)
          }.map(_.sum)
        }

      def subscriptionCost(owner: UserId, currency: CurrencyCode, asOf: LocalDate): F[Either[BudgetError, SubscriptionCost]] =
        (plans.schedules(owner), loader.load(owner)).tupled.map { case (ss, snap) =>
          val subs = ss.filter(s => s.active && s.kind == ScheduleKind.Subscription && s.txKind == TxKind.Expense && Periods.nextDue(s).isDefined)
          subs.traverse { s =>
            val ccy = snap.walletsById.get(s.walletId).fold(currency)(_.currency)
            Reports.convert(snap.fx, Periods.monthlyCost(s).toLong, ccy, currency, asOf)
          }.map(ms => SubscriptionCost(currency, ms.sum, ms.sum * 12, subs.size))
        }

      // ---- objectives ----

      def createObjective(owner: UserId, b: NewObjective): F[Either[BudgetError, Objective]] =
        if (b.targetMinor <= 0) Async[F].pure(Left(BudgetError.Invalid("the target must be positive")))
        else
          for {
            id  <- uuid
            now <- Async[F].delay(Instant.now())
            o   <- plans.createObjective(Objective(ObjectiveId.assume(id), owner, b.kind, b.name, b.targetMinor, b.currency, b.dueDate,
                                                   b.counterparty, archived = false, now))
          } yield Right(o)

      def setObjectiveArchived(owner: UserId, id: ObjectiveId, archived: Boolean): F[Option[Objective]] =
        plans.setObjectiveArchived(owner, id, archived)

      def objectives(owner: UserId): F[Either[BudgetError, List[ObjectiveProgress]]] =
        (plans.objectives(owner), loader.load(owner)).tupled.map { case (os, snap) =>
          os.traverse { o =>
            snap.transactions.filter(_.objectiveId.contains(o.id)).traverse { t =>
              val ccy = snap.walletsById.get(t.walletId).fold(o.currency)(_.currency)
              Reports.convert(snap.fx, t.amountMinor, ccy, o.currency, t.date).map(t.kind -> _)
            }.map(Reports.objective(o, _))
          }
        }
    }
}
