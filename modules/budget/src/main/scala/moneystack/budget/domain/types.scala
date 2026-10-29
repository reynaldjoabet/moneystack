package moneystack.budget.domain

import io.github.iltotore.iron.*

import java.util.UUID

type WalletId      = WalletId.T
object WalletId      extends RefinedType[UUID, Pure] {}

type CategoryId    = CategoryId.T
object CategoryId    extends RefinedType[UUID, Pure] {}

type PayeeId       = PayeeId.T
object PayeeId       extends RefinedType[UUID, Pure] {}

type TagId         = TagId.T
object TagId         extends RefinedType[UUID, Pure] {}

type TransactionId = TransactionId.T
object TransactionId extends RefinedType[UUID, Pure] {}

type BudgetId      = BudgetId.T
object BudgetId      extends RefinedType[UUID, Pure] {}

type ScheduleId    = ScheduleId.T
object ScheduleId    extends RefinedType[UUID, Pure] {}

type ObjectiveId   = ObjectiveId.T
object ObjectiveId   extends RefinedType[UUID, Pure] {}

type RuleId        = RuleId.T
object RuleId        extends RefinedType[UUID, Pure] {}
