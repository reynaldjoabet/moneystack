package wealth.portfolio.http

import wealth.domain.*
import wealth.portfolio.domain.*
import wealth.portfolio.service.*
import wealth.platform.HttpSupport

import cats.effect.*
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

/**
 * The caller's own portfolio under `/portfolio`: every read and write is
 * scoped to the authenticated user. Assets, quotes and FX rates form a
 * shared catalogue.
 */
final class PortfolioRoutes[F[_]: Async](
    auth: Request[F] => F[Option[UserId]],
    defaultCurrency: CurrencyCode,
    portfolio: PortfolioService[F],
    planning: PlanningService[F],
    netWorth: NetWorthService[F]
) extends HttpSupport[F, PortfolioError](auth) {

  import PortfolioJson.given

  private val AccountIdVar   = new UuidVar(InvestmentAccountId.assume)
  private val AssetIdVar     = new UuidVar(AssetId.assume)
  private val ActivityIdVar  = new UuidVar(ActivityId.assume)
  private val LimitIdVar     = new UuidVar(LimitId.assume)
  private val GoalIdVar      = new UuidVar(GoalId.assume)
  private val AltAssetIdVar  = new UuidVar(AltAssetId.assume)
  private val LiabilityIdVar = new UuidVar(LiabilityId.assume)

  private object TaxonomyVar {
    def unapply(s: String): Option[Taxonomy] = Taxonomy.option(s)
  }

  private given QueryParamDecoder[InvestmentAccountId] = uuidParam("account id", InvestmentAccountId.assume)
  private given QueryParamDecoder[CurrencyCode]        = QueryParamDecoder[String].emap(s => CurrencyCode.either(s.toUpperCase).leftMap(ParseFailure(_, s)))
  private given QueryParamDecoder[RebalanceMode]       = enumParam(RebalanceMode.parse)
  private given QueryParamDecoder[BigDecimal]          =
    QueryParamDecoder[String].emap(s => Either.catchNonFatal(BigDecimal(s)).leftMap(_ => ParseFailure(s"not a number: $s", s)))

  private object AccountParam  extends OptionalMultiQueryParamDecoderMatcher[InvestmentAccountId]("account")
  private object CurrencyParam extends OptionalValidatingQueryParamDecoderMatcher[CurrencyCode]("currency")
  private object TaxonomyParam extends OptionalQueryParamDecoderMatcher[String]("taxonomy")
  private object CashParam     extends OptionalQueryParamDecoderMatcher[BigDecimal]("cash")
  private object ModeParam     extends OptionalValidatingQueryParamDecoderMatcher[RebalanceMode]("mode")
  private object DryRunParam   extends OptionalQueryParamDecoderMatcher[Boolean]("dryRun")

  protected def failed(err: PortfolioError): F[Response[F]] = {
    import PortfolioError.*
    val body = errorBody(err)
    err match {
      case AccountNotFound(_) | AssetNotFound(_) | ActivityNotFound(_) | GoalNotFound(_) | LimitNotFound(_) |
           AltAssetNotFound(_) | LiabilityNotFound(_)                        => NotFound(body)
      case DuplicateSymbol(_) | InsufficientQuantity(_, _, _, _)             => Conflict(body)
      case ImportFailed(errors)                                              => BadRequest(Map("errors" -> errors).asJson)
      case _                                                                 => BadRequest(body)
    }
  }

  private def currency(p: Option[cats.data.ValidatedNel[ParseFailure, CurrencyCode]])(f: CurrencyCode => F[Response[F]]): F[Response[F]] =
    param(p, defaultCurrency).fold(bad, f)

  private def accounts(p: cats.data.Validated[cats.data.NonEmptyList[ParseFailure], List[InvestmentAccountId]]): Either[String, Option[List[InvestmentAccountId]]] =
    p.toEither.leftMap(_.head.sanitized).map(ids => Option.when(ids.nonEmpty)(ids))

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- accounts ----

    case req @ GET -> Root / "portfolio" / "accounts" =>
      authed(req)(u => portfolio.accounts(u).flatMap(Ok(_)))

    case req @ POST -> Root / "portfolio" / "accounts" =>
      authed(req)(u => decoded[PortfolioService.NewAccount](req)(b => portfolio.createAccount(u, b).flatMap(Created(_))))

    case req @ POST -> Root / "portfolio" / "accounts" / AccountIdVar(id) / "active" =>
      authed(req)(u => decoded[PortfolioJson.ActiveBody](req)(b => portfolio.setAccountActive(u, id, b.active).flatMap(_.fold(NotFound())(Ok(_)))))

    case req @ PUT -> Root / "portfolio" / "accounts" / AccountIdVar(id) / "balances" =>
      authed(req)(u => decoded[PortfolioJson.BalanceBody](req)(b =>
        portfolio.recordBalance(u, BalanceSnapshot(id, b.date, b.balance, b.invested.getOrElse(BigDecimal(0)))).flatMap(respond(_)(Ok(_)))
      ))

    case req @ POST -> Root / "portfolio" / "accounts" / AccountIdVar(id) / "import" :? DryRunParam(dry) =>
      authed(req)(u => req.bodyText.compile.string.flatMap(csv => portfolio.importCsv(u, id, csv, dry.getOrElse(false)).flatMap(respond(_)(Ok(_)))))

    // ---- catalogue & market data ----

    case req @ GET -> Root / "portfolio" / "assets" =>
      authed(req)(_ => portfolio.assets.flatMap(Ok(_)))

    case req @ POST -> Root / "portfolio" / "assets" =>
      authed(req)(_ => decoded[PortfolioService.NewAsset](req)(b => portfolio.createAsset(b).flatMap(respond(_)(Created(_)))))

    case req @ PUT -> Root / "portfolio" / "assets" / AssetIdVar(id) / "classifications" / TaxonomyVar(t) =>
      authed(req)(_ => decoded[List[PortfolioJson.CategoryWeight]](req)(cs =>
        portfolio.classify(id, t, cs.map(c => c.category -> c.weightBps)).flatMap(respond(_)(Ok(_)))
      ))

    case req @ PUT -> Root / "portfolio" / "quotes" =>
      authed(req)(_ => decoded[List[Quote]](req)(qs => portfolio.addQuotes(qs).flatMap(respond(_)(n => Ok(Map("saved" -> n).asJson)))))

    case req @ PUT -> Root / "portfolio" / "fx-rates" =>
      authed(req)(_ => decoded[List[FxRate]](req)(rs => portfolio.addFxRates(rs).flatMap(respond(_)(n => Ok(Map("saved" -> n).asJson)))))

    // ---- activities ----

    case req @ GET -> Root / "portfolio" / "activities" :? AccountParam(acct) +& From(from) +& To(to) =>
      authed(req)(u => accounts(acct).fold(bad, ids =>
        period(from, to)((f, t) => portfolio.activities(u, ids.flatMap(_.headOption), f, t).flatMap(Ok(_)))))

    case req @ POST -> Root / "portfolio" / "activities" =>
      authed(req)(u => decoded[PortfolioService.NewActivity](req)(b => portfolio.addActivity(u, b).flatMap(respond(_)(Created(_)))))

    case req @ PUT -> Root / "portfolio" / "activities" / ActivityIdVar(id) =>
      authed(req)(u => decoded[PortfolioService.NewActivity](req)(b => portfolio.updateActivity(u, id, b).flatMap(respond(_)(Ok(_)))))

    case req @ DELETE -> Root / "portfolio" / "activities" / ActivityIdVar(id) =>
      authed(req)(u => portfolio.deleteActivity(u, id).flatMap(respond(_)(_ => NoContent())))

    // ---- read models ----

    case req @ GET -> Root / "portfolio" / "holdings" :? AccountParam(acct) +& AsOf(d) +& CurrencyParam(c) =>
      authed(req)(u => accounts(acct).fold(bad, ids => asOf(d)(day => currency(c)(ccy => portfolio.holdings(u, ids, day, ccy).flatMap(respond(_)(Ok(_)))))))

    case req @ GET -> Root / "portfolio" / "history" :? AccountParam(acct) +& From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u => accounts(acct).fold(bad, ids => period(from, to)((f, t) => currency(c)(ccy => portfolio.history(u, ids, f, t, ccy).flatMap(respond(_)(Ok(_)))))))

    case req @ GET -> Root / "portfolio" / "performance" :? AccountParam(acct) +& From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u => accounts(acct).fold(bad, ids => period(from, to)((f, t) => currency(c)(ccy => portfolio.performance(u, ids, f, t, ccy).flatMap(respond(_)(Ok(_)))))))

    case req @ GET -> Root / "portfolio" / "income" :? AccountParam(acct) +& From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u => accounts(acct).fold(bad, ids => period(from, to)((f, t) => currency(c)(ccy => portfolio.income(u, ids, f, t, ccy).flatMap(respond(_)(Ok(_)))))))

    case req @ GET -> Root / "portfolio" / "realized" :? AccountParam(acct) +& From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u => accounts(acct).fold(bad, ids => period(from, to)((f, t) => currency(c)(ccy => portfolio.realized(u, ids, f, t, ccy).flatMap(respond(_)(Ok(_)))))))

    case req @ GET -> Root / "portfolio" / "allocation" :? TaxonomyParam(t) +& AsOf(d) +& CurrencyParam(c) =>
      authed(req)(u => asOf(d)(day => currency(c)(ccy => portfolio.allocation(u, t.getOrElse(Analytics.AssetKindTaxonomy), day, ccy).flatMap(respond(_)(Ok(_))))))

    // ---- targets & rebalancing ----

    case req @ PUT -> Root / "portfolio" / "targets" / TaxonomyVar(t) =>
      authed(req)(u => decoded[List[PortfolioJson.CategoryWeight]](req)(cs =>
        portfolio.setTargets(u, t, cs.map(c => c.category -> c.weightBps)).flatMap(respond(_)(Ok(_)))
      ))

    case req @ GET -> Root / "portfolio" / "targets" / TaxonomyVar(t) / "drift" :? AsOf(d) +& CurrencyParam(c) =>
      authed(req)(u => asOf(d)(day => currency(c)(ccy => portfolio.drift(u, t, day, ccy).flatMap(respond(_)(Ok(_))))))

    case req @ GET -> Root / "portfolio" / "targets" / TaxonomyVar(t) / "rebalance" :? CashParam(cash) +& ModeParam(m) +& AsOf(d) +& CurrencyParam(c) =>
      authed(req)(u => param(m, RebalanceMode.BuyOnly).fold(bad, mode =>
        asOf(d)(day => currency(c)(ccy => portfolio.rebalance(u, t, cash.getOrElse(BigDecimal(0)), mode, day, ccy).flatMap(respond(_)(Ok(_)))))))

    // ---- contribution limits ----

    case req @ GET -> Root / "portfolio" / "limits" :? CurrencyParam(c) =>
      authed(req)(u => currency(c)(ccy => portfolio.limits(u, ccy).flatMap(respond(_)(Ok(_)))))

    case req @ POST -> Root / "portfolio" / "limits" =>
      authed(req)(u => decoded[PortfolioService.NewLimit](req)(b => portfolio.createLimit(u, b).flatMap(respond(_)(Created(_)))))

    case req @ DELETE -> Root / "portfolio" / "limits" / LimitIdVar(id) =>
      authed(req)(u => portfolio.deleteLimit(u, id).flatMap(respond(_)(_ => NoContent())))

    // ---- goals & planning ----

    case req @ GET -> Root / "portfolio" / "goals" :? AsOf(d) =>
      authed(req)(u => asOf(d)(day => planning.goals(u, day).flatMap(respond(_)(Ok(_)))))

    case req @ POST -> Root / "portfolio" / "goals" =>
      authed(req)(u => decoded[PlanningService.NewGoal](req)(b => planning.createGoal(u, b).flatMap(respond(_)(Created(_)))))

    case req @ DELETE -> Root / "portfolio" / "goals" / GoalIdVar(id) =>
      authed(req)(u => planning.deleteGoal(u, id).flatMap(respond(_)(_ => NoContent())))

    case req @ POST -> Root / "portfolio" / "planning" / "save-up" :? AsOf(d) =>
      authed(req)(_ => decoded[SaveUpInput](req)(in => asOf(d)(day => respond(planning.saveUp(in, day))(Ok(_)))))

    case req @ POST -> Root / "portfolio" / "planning" / "fire" =>
      authed(req)(_ => decoded[PortfolioJson.FireBody](req)(b => respond(planning.fire(b.input))(Ok(_))))

    // ---- net worth ----

    case req @ GET -> Root / "portfolio" / "alt-assets" =>
      authed(req)(u => netWorth.altAssets(u).flatMap(Ok(_)))

    case req @ POST -> Root / "portfolio" / "alt-assets" =>
      authed(req)(u => decoded[NetWorthService.NewAltAsset](req)(b => netWorth.createAltAsset(u, b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "portfolio" / "alt-assets" / AltAssetIdVar(id) / "active" =>
      authed(req)(u => decoded[PortfolioJson.ActiveBody](req)(b => netWorth.setAltAssetActive(u, id, b.active).flatMap(_.fold(NotFound())(Ok(_)))))

    case req @ PUT -> Root / "portfolio" / "alt-assets" / AltAssetIdVar(id) / "valuations" =>
      authed(req)(u => decoded[PortfolioJson.ValuationBody](req)(b =>
        netWorth.addValuation(u, AltValuation(id, b.date, b.value, b.source.getOrElse(Title.assume("manual")))).flatMap(respond(_)(Ok(_)))
      ))

    case req @ GET -> Root / "portfolio" / "liabilities" =>
      authed(req)(u => netWorth.liabilities(u).flatMap(Ok(_)))

    case req @ POST -> Root / "portfolio" / "liabilities" =>
      authed(req)(u => decoded[NetWorthService.NewLiability](req)(b => netWorth.createLiability(u, b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "portfolio" / "liabilities" / LiabilityIdVar(id) / "active" =>
      authed(req)(u => decoded[PortfolioJson.ActiveBody](req)(b => netWorth.setLiabilityActive(u, id, b.active).flatMap(_.fold(NotFound())(Ok(_)))))

    case req @ PUT -> Root / "portfolio" / "liabilities" / LiabilityIdVar(id) / "statements" =>
      authed(req)(u => decoded[PortfolioJson.StatementBody](req)(b =>
        netWorth.addStatement(u, LiabilityStatement(id, b.date, b.balance)).flatMap(respond(_)(Ok(_)))
      ))

    case req @ GET -> Root / "portfolio" / "liabilities" / LiabilityIdVar(id) / "schedule" =>
      authed(req)(u => netWorth.schedule(u, id).flatMap(respond(_)(Ok(_))))

    case req @ GET -> Root / "portfolio" / "net-worth" :? AsOf(d) +& CurrencyParam(c) =>
      authed(req)(u => asOf(d)(day => currency(c)(ccy => netWorth.netWorth(u, day, ccy).flatMap(respond(_)(Ok(_))))))

    case req @ GET -> Root / "portfolio" / "net-worth" / "history" :? From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u => period(from, to)((f, t) => currency(c)(ccy => netWorth.history(u, f, t, ccy).flatMap(respond(_)(Ok(_))))))
  }
}
