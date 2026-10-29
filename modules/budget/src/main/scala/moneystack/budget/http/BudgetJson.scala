package moneystack.budget.http

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.budget.service.*
import moneystack.platform.FxQuote
import moneystack.platform.JsonSupport.enumCodec

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

import java.time.LocalDate

object BudgetJson {

  export RuleJson.given

  given Codec[WalletKind]     = enumCodec(WalletKind.render, WalletKind.parse)
  given Codec[CategoryKind]   = enumCodec(CategoryKind.render, CategoryKind.parse)
  given Codec[CategorySource] = enumCodec(CategorySource.render, CategorySource.parse)
  given Codec[Recurrence]     = enumCodec(Recurrence.render, Recurrence.parse)
  given Codec[ScheduleKind]   = enumCodec(ScheduleKind.render, ScheduleKind.parse)
  given Codec[Frequency]      = enumCodec(Frequency.render, Frequency.parse)
  given Codec[ObjectiveKind]  = enumCodec(ObjectiveKind.render, ObjectiveKind.parse)
  given Codec[Bucket]         = enumCodec(Bucket.render, Bucket.parse)

  given Encoder[Wallet]                   = deriveEncoder
  given Encoder[MoneyService.WalletView]  = deriveEncoder
  given Encoder[Category]                 = deriveEncoder
  given Encoder[Payee]                    = deriveEncoder
  given Encoder[Tag]                      = deriveEncoder
  given Codec[Split]                      = deriveCodec
  given Encoder[Transaction]              = deriveEncoder
  given Encoder[Rule]                     = deriveEncoder
  given Encoder[MoneyService.RuleRun]     = deriveEncoder
  given Encoder[MoneyService.ImportReport] = deriveEncoder
  given Codec[FxQuote]                    = deriveCodec

  given Codec[CategoryLimit]              = deriveCodec
  given Encoder[Budget]                   = deriveEncoder
  given Encoder[Period]                   = deriveEncoder
  given Encoder[LimitStatus]              = deriveEncoder
  given Encoder[BudgetStatus]             = deriveEncoder
  given Encoder[PlanService.BudgetView]   = deriveEncoder
  given Encoder[PlanService.PeriodSpend]  = deriveEncoder
  given Encoder[Schedule]                 = deriveEncoder
  given Encoder[PlanService.ScheduleView] = deriveEncoder
  given Encoder[PlanService.Upcoming]     = deriveEncoder
  given Encoder[PlanService.SubscriptionCost] = deriveEncoder
  given Encoder[Objective]                = deriveEncoder
  given Encoder[ObjectiveProgress]        = deriveEncoder

  given Encoder[Summary]                  = deriveEncoder
  given Encoder[CategoryTotal]            = deriveEncoder
  given Encoder[TrendPoint]               = deriveEncoder
  given Encoder[CategoryChange]           = deriveEncoder
  given Encoder[FlowLink]                 = deriveEncoder
  given Encoder[WalletBalance]            = deriveEncoder
  given Encoder[InsightService.Balances]  = deriveEncoder
  given Encoder[Streak]                   = deriveEncoder
  given Encoder[Recap] = Encoder.instance { r =>
    deriveEncoder[Recap].apply(r).deepMerge(Json.obj("month" -> r.month.toString.asJson))
  }
  given Encoder[(LocalDate, Long)] = Encoder.instance { case (d, v) => Json.obj("date" -> d.asJson, "balanceMinor" -> v.asJson) }

  given Decoder[MoneyService.NewWallet]      = deriveDecoder
  given Decoder[MoneyService.NewCategory]    = deriveDecoder
  given Decoder[MoneyService.NewPayee]       = deriveDecoder
  given Decoder[MoneyService.NewTransaction] = deriveDecoder
  given Decoder[MoneyService.NewRule]        = deriveDecoder
  given Decoder[PlanService.NewBudget]       = deriveDecoder
  given Decoder[PlanService.NewSchedule]     = deriveDecoder
  given Decoder[PlanService.NewObjective]    = deriveDecoder

  final case class ArchiveBody(archived: Boolean)
  given Decoder[ArchiveBody] = deriveDecoder

  final case class ActiveBody(active: Boolean)
  given Decoder[ActiveBody] = deriveDecoder

  final case class ShareBody(userId: UserId)
  given Decoder[ShareBody] = deriveDecoder

  final case class TagBody(name: Title)
  given Decoder[TagBody] = deriveDecoder

  final case class PayBody(date: Option[LocalDate], amountMinor: Option[Long])
  given Decoder[PayBody] = deriveDecoder
}
