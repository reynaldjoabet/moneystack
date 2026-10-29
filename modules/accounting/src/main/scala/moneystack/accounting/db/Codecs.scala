package moneystack.accounting.db

import moneystack.domain.*
import moneystack.accounting.domain.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.skunk.*

import _root_.skunk.*
import _root_.skunk.codec.all.*

object Codecs {

  // ---- shared scalars ----
  export moneystack.platform.CoreCodecs.{amountMinor, body, bps, currency, decimal, email, instant, priceMinor, quantity, title, userId}

  // ---- identifiers ----
  val accountId: Codec[AccountId]                 = uuid.imap(AccountId.assume)(_.value)
  val journalEntryId: Codec[JournalEntryId]       = uuid.imap(JournalEntryId.assume)(_.value)
  val contactId: Codec[ContactId]                 = uuid.imap(ContactId.assume)(_.value)
  val taxRateId: Codec[TaxRateId]                 = uuid.imap(TaxRateId.assume)(_.value)
  val itemId: Codec[ItemId]                       = uuid.imap(ItemId.assume)(_.value)
  val adjustmentId: Codec[AdjustmentId]           = uuid.imap(AdjustmentId.assume)(_.value)
  val documentId: Codec[DocumentId]               = uuid.imap(DocumentId.assume)(_.value)
  val settlementId: Codec[SettlementId]           = uuid.imap(SettlementId.assume)(_.value)
  val bankTransactionId: Codec[BankTransactionId] = uuid.imap(BankTransactionId.assume)(_.value)
  val bankRuleId: Codec[BankRuleId]               = uuid.imap(BankRuleId.assume)(_.value)

  // ---- accounting scalars ----
  val accountCode: Codec[AccountCode]   = varchar(16).refined[AccountCodeConstraint].imap(AccountCode.assume)(_.value)
  val journalRef: Codec[JournalRef]     = varchar(64).refined[JournalRefConstraint].imap(JournalRef.assume)(_.value)
  val taxCode: Codec[TaxCode]           = varchar(16).refined[TaxCodeConstraint].imap(TaxCode.assume)(_.value)
  val sku: Codec[Sku]                   = varchar(32).refined[SkuConstraint].imap(Sku.assume)(_.value)
  val exchangeRate: Codec[ExchangeRate] = numeric(24, 10).eimap[ExchangeRate](ExchangeRate.positive)(_.value)

  // ---- enums ----
  val accountType: Codec[AccountType]     = varchar(32).eimap(AccountType.parse)(AccountType.render)
  val side: Codec[Side]                   = varchar(6).eimap(Side.parse)(Side.render)
  val journalSource: Codec[JournalSource] = varchar(24).eimap(JournalSource.parse)(JournalSource.render)
  val contactKind: Codec[ContactKind]     = varchar(16).eimap(ContactKind.parse)(ContactKind.render)
  val itemKind: Codec[ItemKind]           = varchar(16).eimap(ItemKind.parse)(ItemKind.render)
  val documentKind: Codec[DocumentKind]   = varchar(8).eimap(DocumentKind.parse)(DocumentKind.render)
  val bankTxStatus: Codec[BankTxStatus]   = varchar(16).eimap(BankTxStatus.parse)(BankTxStatus.render)
  val ruleApplyTo: Codec[RuleApplyTo]     = varchar(16).eimap(RuleApplyTo.parse)(RuleApplyTo.render)
  val conjunction: Codec[Conjunction]     = varchar(8).eimap(Conjunction.parse)(Conjunction.render)
  val ruleField: Codec[RuleField]         = varchar(16).eimap(RuleField.parse)(RuleField.render)
  val ruleOperator: Codec[RuleOperator]   = varchar(16).eimap(RuleOperator.parse)(RuleOperator.render)

  // ---- ledger ----

  val account
      : Codec[Account] = (accountId *: accountCode *: title *: accountType *: currency *: accountId.opt *: bool *: instant).to[Account]

  val journalHeader: Codec[JournalHeader] =
    (journalEntryId *: date *: journalRef.opt *: body.opt *: currency *: exchangeRate *:
      journalSource *: uuid.opt *: userId.opt *: instant).to[JournalHeader]

  val postedLine: Codec[PostedLine] = (int4 *: accountId *: side *: priceMinor *: amountMinor *: contactId.opt).to[PostedLine]

  val fxRate: Codec[FxRate] = (currency *: date *: exchangeRate).to[FxRate]

  val accountTotals: Codec[AccountTotals] = (accountId *: int8 *: int8).to[AccountTotals]

  val ledgerRow: Codec[LedgerRow] = (journalEntryId *: date *: journalRef.opt *: body.opt *: accountId *: side *: amountMinor).to[LedgerRow]

  val ledgerLock: Codec[LedgerLock] = (date *: date.opt *: date.opt *: body *: userId *: instant).to[LedgerLock]

  /** The single `ledger_lock` row; all-NULL columns mean the books are open. */
  val ledgerLockRow: Decoder[Option[LedgerLock]] = (date.opt *: date.opt *: date.opt *: body.opt *: userId.opt *: instant.opt).map {
    case (Some(through), from, to, Some(reason), Some(by), Some(at)) =>
      Some(LedgerLock(through, from, to, reason, by, at))
    case _ => None
  }

  // ---- catalog ----

  val contact: Codec[Contact] = (contactId *: contactKind *: title *: email.opt *: currency *: userId.opt *: bool *: instant).to[Contact]

  val taxRate: Codec[TaxRate] = (taxRateId *: taxCode *: title *: bps *: bool *: instant).to[TaxRate]

  val item: Codec[Item] =
    (itemId *: sku *: title *: itemKind *: priceMinor.opt *: priceMinor.opt *: accountId *: accountId *:
      accountId.opt *: bool *: instant).to[Item]

  val stock: Codec[Stock] = (itemId *: decimal *: int8).to[Stock]

  val inventoryMove: Codec[InventoryMove] = (itemId *: date *: decimal *: int8 *: journalSource *: uuid).to[InventoryMove]

  val inventoryAdjustment: Codec[InventoryAdjustment] =
    (adjustmentId *: itemId *: date *: decimal *: int8 *: accountId *: body *: journalEntryId *:
      userId.opt *: instant).to[InventoryAdjustment]

  // ---- documents & settlements ----

  val document: Codec[Document] =
    (documentId *: documentKind *: varchar(32) *: contactId *: date *: date *: currency *: exchangeRate *:
      journalRef.opt *: body.opt *: amountMinor *: amountMinor *: priceMinor *: amountMinor *: journalEntryId *:
      date.opt *: userId.opt *: instant).to[Document]

  val documentLine: Codec[DocumentLine] =
    (int4 *: itemId.opt *: body.opt *: quantity *: priceMinor *: accountId *: taxRateId.opt *: bps.opt *:
      amountMinor *: amountMinor).to[DocumentLine]

  val settlement: Codec[Settlement] =
    (settlementId *: documentKind *: varchar(32) *: contactId *: date *: currency *: exchangeRate *: accountId *:
      journalRef.opt *: body.opt *: priceMinor *: journalEntryId *: journalEntryId.opt *: date.opt *:
      userId.opt *: instant).to[Settlement]

  val allocation: Codec[Allocation] = (documentId *: priceMinor *: amountMinor *: amountMinor).to[Allocation]

  val openItem
      : Codec[OpenItem] = (documentId *: varchar(32) *: contactId *: date *: date *: currency *: int8 *: int8 *: int8 *: int8).to[OpenItem]

  // ---- banking ----

  val bankTransaction: Codec[BankTransaction] =
    (bankTransactionId *: accountId *: date *: title *: int8 *: varchar(128).opt *: bankTxStatus *:
      bankRuleId.opt *: accountId.opt *: contactId.opt *: journalEntryId.opt *: uuid.opt *: instant).to[BankTransaction]

  val ruleCondition: Codec[RuleCondition] = (ruleField *: ruleOperator *: varchar(200)).to[RuleCondition]

  /** A rule without its conditions, which live in `bank_rule_conditions`. */
  val bankRule: Codec[BankRule] =
    (bankRuleId *: title *: int4 *: ruleApplyTo *: conjunction *: accountId *: contactId.opt *: bool *: bool *: instant)
      .imap { case (id, name, priority, applyTo, conj, account, contact, auto, active, at) =>
        BankRule(id, name, priority, applyTo, conj, Nil, account, contact, auto, active, at)
      } { r =>
        (r.id, r.name, r.priority, r.applyTo, r.conjunction, r.accountId, r.contactId, r.autoCategorize, r.active, r.createdAt)
      }

  // ---- workflow & reconciliation ----

  val draftId: Codec[DraftId]                   = uuid.imap(DraftId.assume)(_.value)
  val recurringId: Codec[RecurringId]           = uuid.imap(RecurringId.assume)(_.value)
  val reconciliationId: Codec[ReconciliationId] = uuid.imap(ReconciliationId.assume)(_.value)

  val eventKind: Codec[DocumentEventKind]      = varchar(16).eimap(DocumentEventKind.parse)(DocumentEventKind.render)
  val every: Codec[Every]                      = varchar(8).eimap(Every.parse)(Every.render)
  val reconStatus: Codec[ReconciliationStatus] = varchar(8).eimap(ReconciliationStatus.parse)(ReconciliationStatus.render)

  val draft: Codec[Draft] = (draftId *: documentKind *: contactId *: text *: userId *: instant *: instant).to[Draft]

  val documentEvent: Codec[DocumentEvent] = (documentId *: eventKind *: body.opt *: userId.opt *: instant).to[DocumentEvent]

  val recurring: Codec[Recurring] =
    (recurringId *: documentKind *: contactId *: text *: every *: int4 *: date *: date.opt *: int4.opt *: int4 *: bool *:
      userId *: instant).to[Recurring]

  val reconciliation: Codec[Reconciliation] =
    (reconciliationId *: accountId *: date *: date *: int8 *: int8 *: reconStatus *: userId *: instant *: instant.opt).to[Reconciliation]
}
