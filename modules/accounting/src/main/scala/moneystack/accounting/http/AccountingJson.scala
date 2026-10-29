package moneystack.accounting.http

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.service.*

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

import java.time.LocalDate

object AccountingJson {

  private def enumCodec[A](render: A => String, parse: String => Either[String, A]): Codec[A] =
    Codec.from(Decoder.decodeString.emap(parse), Encoder.encodeString.contramap(render))

  // ---- enums ----
  given Codec[AccountType]      = enumCodec(AccountType.render, AccountType.parse)
  given Codec[Side]             = enumCodec(Side.render, Side.parse)
  given Codec[JournalSource]    = enumCodec(JournalSource.render, JournalSource.parse)
  given Codec[ContactKind]      = enumCodec(ContactKind.render, ContactKind.parse)
  given Codec[ItemKind]         = enumCodec(ItemKind.render, ItemKind.parse)
  given Codec[DocumentKind]     = enumCodec(DocumentKind.render, DocumentKind.parse)
  given Codec[BankTxStatus]     = enumCodec(BankTxStatus.render, BankTxStatus.parse)
  given Codec[RuleApplyTo]      = enumCodec(RuleApplyTo.render, RuleApplyTo.parse)
  given Codec[Conjunction]      = enumCodec(Conjunction.render, Conjunction.parse)
  given Codec[RuleField]        = enumCodec(RuleField.render, RuleField.parse)
  given Codec[RuleOperator]     = enumCodec(RuleOperator.render, RuleOperator.parse)
  given Encoder[RootType]       = Encoder.encodeString.contramap(_.toString.toLowerCase)
  given Encoder[DocumentStatus] = Encoder.encodeString.contramap {
    case DocumentStatus.Open          => "open"
    case DocumentStatus.PartiallyPaid => "partially_paid"
    case DocumentStatus.Paid          => "paid"
    case DocumentStatus.Void          => "void"
  }

  /** Positive-only decimals; the refined types themselves only carry `Pure`. */
  given Decoder[ExchangeRate] = Decoder.decodeBigDecimal.emap(ExchangeRate.positive)
  given Decoder[Quantity]     = Decoder.decodeBigDecimal.emap(Quantity.positive)

  // ---- ledger ----
  given Encoder[Account]       = deriveEncoder
  given Encoder[PostedLine]    = deriveEncoder
  given Encoder[JournalHeader] = deriveEncoder
  given Encoder[JournalEntry]  = Encoder.instance { e =>
    Encoder[JournalHeader].apply(e.header).deepMerge(Json.obj("lines" -> Encoder[List[PostedLine]].apply(e.lines)))
  }
  given Encoder[FxRate]     = deriveEncoder
  given Encoder[LedgerLock] = deriveEncoder

  given Encoder[FinancialReports.Line]                 = deriveEncoder
  given Encoder[FinancialReports.Section]              = deriveEncoder
  given Encoder[FinancialReports.TrialBalanceRow]      = deriveEncoder
  given Encoder[FinancialReports.TrialBalance]         = deriveEncoder
  given Encoder[FinancialReports.ProfitAndLoss]        = deriveEncoder
  given Encoder[FinancialReports.BalanceSheet]         = deriveEncoder
  given Encoder[FinancialReports.GeneralLedgerLine]    = deriveEncoder
  given Encoder[FinancialReports.GeneralLedgerAccount] = deriveEncoder
  given Encoder[FinancialReports.GeneralLedger]        = deriveEncoder

  // ---- catalog ----
  given Encoder[Contact] = deriveEncoder
  given Encoder[TaxRate] = deriveEncoder
  given Encoder[Item]    = deriveEncoder

  // ---- documents, settlements, inventory ----
  given Encoder[DocumentLine]                 = deriveEncoder
  given Encoder[Document]                     = deriveEncoder
  given Encoder[DocumentDetails]              = deriveEncoder
  given Encoder[TradeService.DocumentSummary] = deriveEncoder
  given Encoder[Allocation]                   = deriveEncoder
  given Encoder[Settlement]                   = deriveEncoder
  given Encoder[SettlementDetails]            = deriveEncoder
  given Encoder[InventoryAdjustment]          = deriveEncoder

  given Encoder[TradeReports.AgingBuckets]       = deriveEncoder
  given Encoder[TradeReports.AgingDocument]      = deriveEncoder
  given Encoder[TradeReports.AgingRow]           = deriveEncoder
  given Encoder[TradeReports.Aging]              = deriveEncoder
  given Encoder[TradeReports.SalesTaxRow]        = deriveEncoder
  given Encoder[TradeReports.SalesTax]           = deriveEncoder
  given Encoder[TradeReports.InventoryRow]       = deriveEncoder
  given Encoder[TradeReports.InventoryValuation] = deriveEncoder

  // ---- banking ----
  given Codec[RuleCondition]                 = deriveCodec
  given Encoder[BankRule]                    = deriveEncoder
  given Encoder[BankTransaction]             = deriveEncoder
  given Encoder[BankingService.ImportResult] = Encoder.instance { r =>
    Json.obj(
      "imported"    -> Json.fromInt(r.imported),
      "duplicates"  -> Json.fromInt(r.duplicates),
      "matched"     -> Json.fromInt(r.matched),
      "categorized" -> Json.fromInt(r.categorized),
      "failed"      -> Json.arr(r.failed.map { case (id, err) =>
        Json.obj("id" -> Json.fromString(id.value.toString), "error" -> Json.fromString(err))
      }*)
    )
  }

  // ---- request bodies ----
  final case class CreateAccountBody(
      code: AccountCode,
      name: Title,
      accountType: AccountType,
      currency: Option[CurrencyCode],
      parentId: Option[AccountId]
  )
  given Decoder[CreateAccountBody] = deriveDecoder

  final case class ActiveBody(active: Boolean)
  given Decoder[ActiveBody] = deriveDecoder

  final case class DateBody(date: LocalDate)
  given Decoder[DateBody] = deriveDecoder

  given Decoder[JournalLine] = deriveDecoder
  final case class PostJournalBody(
      date: LocalDate,
      reference: Option[JournalRef],
      memo: Option[Body],
      currency: CurrencyCode,
      exchangeRate: Option[ExchangeRate],
      lines: List[JournalLine]
  )
  given Decoder[PostJournalBody] = deriveDecoder

  final case class SetRateBody(currency: CurrencyCode, date: LocalDate, rate: ExchangeRate)
  given Decoder[SetRateBody] = deriveDecoder

  final case class SetLockBody(lockThrough: LocalDate, unlockFrom: Option[LocalDate], unlockTo: Option[LocalDate], reason: Body)
  given Decoder[SetLockBody] = deriveDecoder

  given Decoder[CatalogService.NewContact] = deriveDecoder
  given Decoder[CatalogService.NewTaxRate] = deriveDecoder
  given Decoder[CatalogService.NewItem]    = deriveDecoder

  given Decoder[TradeService.NewLine]       = deriveDecoder
  given Decoder[TradeService.NewDocument]   = deriveDecoder
  given Decoder[TradeService.NewAllocation] = deriveDecoder
  given Decoder[TradeService.NewSettlement] = deriveDecoder
  given Decoder[TradeService.NewAdjustment] = deriveDecoder

  given Decoder[BankingService.NewRule]        = deriveDecoder
  given Decoder[BankingService.NewTransaction] = deriveDecoder

  final case class CategorizeBody(accountId: AccountId, contactId: Option[ContactId])
  given Decoder[CategorizeBody] = deriveDecoder

  // ---- workflow & cash ----
  given Codec[Every]                = enumCodec(Every.render, Every.parse)
  given Codec[DocumentEventKind]    = enumCodec(DocumentEventKind.render, DocumentEventKind.parse)
  given Codec[ReconciliationStatus] = enumCodec(ReconciliationStatus.render, ReconciliationStatus.parse)

  given Encoder[TradeService.NewDocument] = {
    import WorkflowService.Payload.given
    summon[Codec[TradeService.NewDocument]]
  }
  given Encoder[Draft] = Encoder.instance(d =>
    Json.obj(
      "id"        -> d.id.asJson,
      "kind"      -> d.kind.asJson,
      "contactId" -> d.contactId.asJson,
      "createdBy" -> d.createdBy.asJson,
      "createdAt" -> d.createdAt.asJson,
      "updatedAt" -> d.updatedAt.asJson
    )
  )
  given Encoder[WorkflowService.DraftView] = deriveEncoder
  given Encoder[DocumentEvent]             = deriveEncoder
  given Encoder[Recurring]                 = Encoder.instance(r =>
    Json.obj(
      "id"             -> r.id.asJson,
      "kind"           -> r.kind.asJson,
      "contactId"      -> r.contactId.asJson,
      "every"          -> r.every.asJson,
      "interval"       -> r.interval.asJson,
      "startDate"      -> r.startDate.asJson,
      "endDate"        -> r.endDate.asJson,
      "maxOccurrences" -> r.maxOccurrences.asJson,
      "issued"         -> r.nextIndex.asJson,
      "active"         -> r.active.asJson,
      "createdAt"      -> r.createdAt.asJson
    )
  )
  given Encoder[WorkflowService.RecurringView] = deriveEncoder
  given Encoder[WorkflowService.RunReport]     = Encoder.instance(r =>
    Json.obj(
      "issued" -> r.issued.asJson,
      "failed" -> r.failed.map { case (id, e) => Json.obj("recurringId" -> id.asJson, "error" -> e.asJson) }.asJson
    )
  )
  given Encoder[Reconciliation]     = deriveEncoder
  given Encoder[ReconLine]          = deriveEncoder
  given Encoder[ReconciliationView] = deriveEncoder

  given Decoder[WorkflowService.NewRecurring]  = deriveDecoder
  given Decoder[CashService.NewMovement]       = deriveDecoder
  given Decoder[CashService.NewTransfer]       = deriveDecoder
  given Decoder[CashService.NewReconciliation] = deriveDecoder

  final case class NoteBody(note: Option[Body])
  given Decoder[NoteBody] = deriveDecoder

  final case class LineRef(entryId: JournalEntryId, lineNo: Int)
  given Decoder[LineRef] = deriveDecoder
  final case class ClearBody(lines: List[LineRef], cleared: Boolean)
  given Decoder[ClearBody] = deriveDecoder
}
