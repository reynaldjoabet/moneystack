package moneystack.portfolio.http

import moneystack.domain.*
import moneystack.portfolio.domain.*
import moneystack.portfolio.service.*
import moneystack.platform.JsonSupport.enumCodec

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

import java.time.LocalDate

object PortfolioJson {

  // ---- enums ----
  given Codec[AccountKind]      = enumCodec(AccountKind.render, AccountKind.parse)
  given Codec[Tracking]         = enumCodec(Tracking.render, Tracking.parse)
  given Codec[AssetKind]        = enumCodec(AssetKind.render, AssetKind.parse)
  given Codec[ActivityType]     = enumCodec(ActivityType.render, ActivityType.parse)
  given Codec[AltAssetKind]     = enumCodec(AltAssetKind.render, AltAssetKind.parse)
  given Codec[VehicleClass]     = enumCodec(VehicleClass.render, VehicleClass.parse)
  given Codec[DepreciationPace] = enumCodec(DepreciationPace.render, DepreciationPace.parse)
  given Codec[LiabilityKind]    = enumCodec(LiabilityKind.render, LiabilityKind.parse)
  given Codec[RebalanceMode]    = enumCodec(RebalanceMode.render, RebalanceMode.parse)

  // ---- entities ----
  given Encoder[InvestmentAccount] = deriveEncoder
  given Codec[BalanceSnapshot]     = deriveCodec
  given Encoder[Asset]             = deriveEncoder
  given Encoder[Classification]    = deriveEncoder
  given Codec[Quote]               = deriveCodec
  given Codec[FxRate]              = deriveCodec
  given Encoder[Activity]          = deriveEncoder
  given Encoder[AllocationTarget]  = deriveEncoder
  given Encoder[ContributionLimit] = deriveEncoder
  given Codec[GoalFunding]         = deriveCodec
  given Encoder[Goal]              = deriveEncoder
  given Encoder[AltAsset]          = deriveEncoder
  given Codec[AltValuation]        = deriveCodec
  given Encoder[Liability]         = deriveEncoder
  given Codec[LiabilityStatement]  = deriveCodec

  // ---- read models ----
  given Encoder[HoldingView]                        = deriveEncoder
  given Encoder[PortfolioService.Holdings]          = deriveEncoder
  given Encoder[DailyValue]                         = deriveEncoder
  given Encoder[Returns]                            = deriveEncoder
  given Encoder[ReturnPoint]                        = deriveEncoder
  given Encoder[PortfolioService.PerformanceReport] = deriveEncoder
  given Encoder[Disposal]                           = Encoder.instance { d =>
    deriveEncoder[Disposal].apply(d).deepMerge(Json.obj("realized" -> d.realized.asJson, "realizedBase" -> d.realizedBase.asJson))
  }
  given Encoder[PortfolioService.RealizedReport] = deriveEncoder
  given Encoder[IncomeSummary]                   = Encoder.instance { s =>
    Json.obj(
      "total"                -> s.total.asJson,
      "dividends"            -> s.dividends.asJson,
      "interest"             -> s.interest.asJson,
      "trailingTwelveMonths" -> s.trailingTwelveMonths.asJson,
      "byMonth"              -> s.byMonth.map { case (m, v) => Json.obj("month" -> m.toString.asJson, "amount" -> v.asJson) }.asJson,
      "byAsset"              -> s.byAsset.map { case (a, v) => Json.obj("assetId" -> a.asJson, "amount" -> v.asJson) }.asJson
    )
  }
  given Encoder[Slice]                         = deriveEncoder
  given Encoder[DriftRow]                      = deriveEncoder
  given Encoder[RebalanceRow]                  = deriveEncoder
  given Encoder[LimitProgress]                 = deriveEncoder
  given Encoder[PortfolioService.ImportReport] = deriveEncoder

  given Encoder[SaveUpPoint] = Encoder.instance { p =>
    Json.obj(
      "month"       -> p.month.toString.asJson,
      "nominal"     -> p.nominal.asJson,
      "optimistic"  -> p.optimistic.asJson,
      "pessimistic" -> p.pessimistic.asJson,
      "target"      -> p.target.asJson
    )
  }
  given Encoder[SaveUpOverview]               = deriveEncoder
  given Encoder[PlanningService.GoalProgress] = deriveEncoder
  given Encoder[FireYear]                     = deriveEncoder
  given Encoder[FirePercentiles]              = deriveEncoder
  given Encoder[FireResult]                   = deriveEncoder

  given Encoder[AmortizationRow]          = deriveEncoder
  given Encoder[NetWorthService.Schedule] = deriveEncoder
  given Encoder[NetWorthService.Line]     = deriveEncoder
  given Encoder[NetWorthService.NetWorth] = deriveEncoder
  given Encoder[NetWorthService.Point]    = deriveEncoder

  // ---- request bodies ----
  given Decoder[PortfolioService.NewAccount]  = deriveDecoder
  given Decoder[PortfolioService.NewAsset]    = deriveDecoder
  given Decoder[PortfolioService.NewActivity] = deriveDecoder
  given Decoder[PortfolioService.NewLimit]    = deriveDecoder
  given Decoder[PlanningService.NewGoal]      = deriveDecoder
  given Decoder[SaveUpInput]                  = deriveDecoder
  given Decoder[NetWorthService.NewAltAsset]  = deriveDecoder
  given Decoder[NetWorthService.NewLiability] = deriveDecoder

  final case class BalanceBody(date: LocalDate, balance: BigDecimal, invested: Option[BigDecimal])
  given Decoder[BalanceBody] = deriveDecoder

  final case class CategoryWeight(category: Title, weightBps: Bps)
  given Decoder[CategoryWeight] = deriveDecoder

  final case class ValuationBody(date: LocalDate, value: BigDecimal, source: Option[Title])
  given Decoder[ValuationBody] = deriveDecoder

  final case class StatementBody(date: LocalDate, balance: BigDecimal)
  given Decoder[StatementBody] = deriveDecoder

  final case class ActiveBody(active: Boolean)
  given Decoder[ActiveBody] = deriveDecoder

  /** FIRE inputs with defaults: 7% return, 15% volatility, 2.5% inflation, 4% withdrawal, age 90, 1000 runs. */
  final case class FireBody(
      currentPortfolio: BigDecimal,
      annualExpenses: BigDecimal,
      annualSavings: BigDecimal,
      currentAge: Int,
      expectedReturn: Option[BigDecimal],
      returnVolatility: Option[BigDecimal],
      inflation: Option[BigDecimal],
      withdrawalRate: Option[BigDecimal],
      retirementAge: Option[Int],
      horizonAge: Option[Int],
      simulations: Option[Int],
      seed: Option[Long]
  ) {
    def input: FireInput = FireInput(
      currentPortfolio,
      annualExpenses,
      annualSavings,
      expectedReturn.getOrElse(BigDecimal("0.07")),
      returnVolatility.getOrElse(BigDecimal("0.15")),
      inflation.getOrElse(BigDecimal("0.025")),
      withdrawalRate.getOrElse(BigDecimal("0.04")),
      currentAge,
      retirementAge,
      horizonAge.getOrElse(90),
      simulations.getOrElse(1000),
      seed.getOrElse(42L)
    )
  }
  given Decoder[FireBody] = deriveDecoder
}
