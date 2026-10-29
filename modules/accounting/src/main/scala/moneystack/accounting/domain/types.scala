package moneystack.accounting.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import java.util.UUID

// ---------- Identifiers ----------

type AccountId         = AccountId.T
object AccountId         extends RefinedType[UUID, Pure] {}

type JournalEntryId    = JournalEntryId.T
object JournalEntryId    extends RefinedType[UUID, Pure] {}

type ContactId         = ContactId.T
object ContactId         extends RefinedType[UUID, Pure] {}

type TaxRateId         = TaxRateId.T
object TaxRateId         extends RefinedType[UUID, Pure] {}

type ItemId            = ItemId.T
object ItemId            extends RefinedType[UUID, Pure] {}

type AdjustmentId      = AdjustmentId.T
object AdjustmentId      extends RefinedType[UUID, Pure] {}

type DocumentId        = DocumentId.T
object DocumentId        extends RefinedType[UUID, Pure] {}

type SettlementId      = SettlementId.T
object SettlementId      extends RefinedType[UUID, Pure] {}

type BankTransactionId = BankTransactionId.T
object BankTransactionId extends RefinedType[UUID, Pure] {}

type BankRuleId        = BankRuleId.T
object BankRuleId        extends RefinedType[UUID, Pure] {}

type DraftId           = DraftId.T
object DraftId           extends RefinedType[UUID, Pure] {}

type RecurringId       = RecurringId.T
object RecurringId       extends RefinedType[UUID, Pure] {}

type ReconciliationId  = ReconciliationId.T
object ReconciliationId  extends RefinedType[UUID, Pure] {}

// ---------- Scalars ----------

/** Chart-of-accounts code: 3-6 digits with an optional sub-account suffix (e.g. 4000, 4000-01). */
type AccountCodeConstraint = Match["^[0-9]{3,6}(-[0-9]{1,4})?$"]
type AccountCode = AccountCode.T
object AccountCode extends RefinedType[String, AccountCodeConstraint] {}

/** External reference on a journal entry or document (invoice number, PSP ref, ...). */
type JournalRefConstraint = Not[Blank] & MaxLength[64]
type JournalRef = JournalRef.T
object JournalRef extends RefinedType[String, JournalRefConstraint] {}

/** Tax rate code, e.g. VAT20 or GST. */
type TaxCodeConstraint = Match["^[A-Z0-9_-]{1,16}$"]
type TaxCode = TaxCode.T
object TaxCode extends RefinedType[String, TaxCodeConstraint] {}

/** Stock-keeping unit. */
type SkuConstraint = Match["^[A-Za-z0-9._-]{1,32}$"]
type Sku = Sku.T
object Sku extends RefinedType[String, SkuConstraint] {}

/**
 * Units of base currency per one unit of a foreign currency. Positive,
 * enforced via the smart constructor like `Quantity`.
 */
type ExchangeRate = ExchangeRate.T
object ExchangeRate extends RefinedType[BigDecimal, Pure] {
  val One: ExchangeRate = ExchangeRate.assume(BigDecimal(1))
  def positive(b: BigDecimal): Either[String, ExchangeRate] =
    if (b > 0) Right(ExchangeRate.assume(b)) else Left(s"exchange rate must be > 0: $b")
}
