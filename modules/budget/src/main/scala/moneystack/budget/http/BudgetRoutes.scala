package moneystack.budget.http

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.budget.service.*
import moneystack.platform.{FxQuote, HttpSupport}

import cats.data.ValidatedNel
import cats.effect.*
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`

import java.time.{LocalDate, YearMonth}

/** The caller's budget under `/budget`; shared wallets are readable but only their owners write. */
final class BudgetRoutes[F[_]: Async](
    auth: Request[F] => F[Option[UserId]],
    defaultCurrency: CurrencyCode,
    money: MoneyService[F],
    plans: PlanService[F],
    insights: InsightService[F]
) extends HttpSupport[F, BudgetError](auth) {

  import BudgetJson.given

  private val WalletIdVar      = new UuidVar(WalletId.assume)
  private val CategoryIdVar    = new UuidVar(CategoryId.assume)
  private val TransactionIdVar = new UuidVar(TransactionId.assume)
  private val RuleIdVar        = new UuidVar(RuleId.assume)
  private val BudgetIdVar      = new UuidVar(BudgetId.assume)
  private val ScheduleIdVar    = new UuidVar(ScheduleId.assume)
  private val ObjectiveIdVar   = new UuidVar(ObjectiveId.assume)
  private val UserIdVar        = new UuidVar(UserId.assume)

  private given QueryParamDecoder[WalletId]     = uuidParam("wallet id", WalletId.assume)
  private given QueryParamDecoder[CategoryId]   = uuidParam("category id", CategoryId.assume)
  private given QueryParamDecoder[CurrencyCode] =
    QueryParamDecoder[String].emap(s => CurrencyCode.either(s.toUpperCase).leftMap(ParseFailure(_, s)))
  private given QueryParamDecoder[CategoryKind] = enumParam(CategoryKind.parse)
  private given QueryParamDecoder[Bucket]       = enumParam(Bucket.parse)
  private given QueryParamDecoder[YearMonth]    =
    QueryParamDecoder[String].emap(s =>
      Either.catchNonFatal(YearMonth.parse(s)).leftMap(_ => ParseFailure(s"invalid month: $s (expected YYYY-MM)", s))
    )

  private object WalletParam   extends OptionalMultiQueryParamDecoderMatcher[WalletId]("wallet")
  private object CategoryParam extends OptionalValidatingQueryParamDecoderMatcher[CategoryId]("category")
  private object SearchParam   extends OptionalQueryParamDecoderMatcher[String]("q")
  private object CurrencyParam extends OptionalValidatingQueryParamDecoderMatcher[CurrencyCode]("currency")
  private object KindParam     extends OptionalValidatingQueryParamDecoderMatcher[CategoryKind]("kind")
  private object BucketParam   extends OptionalValidatingQueryParamDecoderMatcher[Bucket]("bucket")
  private object MonthParam    extends OptionalValidatingQueryParamDecoderMatcher[YearMonth]("month")
  private object DaysParam     extends OptionalQueryParamDecoderMatcher[Int]("days")
  private object PeriodsParam  extends OptionalQueryParamDecoderMatcher[Int]("periods")
  private object DryRunParam   extends OptionalQueryParamDecoderMatcher[Boolean]("dryRun")
  private object ApplyParam    extends OptionalQueryParamDecoderMatcher[Boolean]("apply")

  protected def failed(err: BudgetError): F[Response[F]] = {
    import BudgetError.*
    val body = errorBody(err)
    err match {
      case WalletNotFound(_) | CategoryNotFound(_) | PayeeNotFound(_) | TagNotFound(_) | TransactionNotFound(_) | BudgetNotFound(_) |
          ScheduleNotFound(_) | ObjectiveNotFound(_) | RuleNotFound(_) => NotFound(body)
      case ReadOnlyWallet(_)                           => Forbidden(body)
      case ScheduleFinished(_) | AlreadyRecorded(_, _) => Conflict(body)
      case ImportFailed(errors)                        => BadRequest(Map("errors" -> errors).asJson)
      case _                                           => BadRequest(body)
    }
  }

  private def currency(p: Option[ValidatedNel[ParseFailure, CurrencyCode]])(f: CurrencyCode => F[Response[F]]): F[Response[F]] =
    param(p, defaultCurrency).fold(bad, f)

  private def walletsOf(p: cats.data.Validated[cats.data.NonEmptyList[ParseFailure], List[WalletId]])
      : Either[String, Option[List[WalletId]]] =
    p.toEither.leftMap(_.head.sanitized).map(ws => Option.when(ws.nonEmpty)(ws))

  /** Period, currency and wallet filter, the arguments most insight routes share. */
  private def scoped(
      from: Option[ValidatedNel[ParseFailure, LocalDate]],
      to: Option[ValidatedNel[ParseFailure, LocalDate]],
      c: Option[ValidatedNel[ParseFailure, CurrencyCode]],
      w: cats.data.Validated[cats.data.NonEmptyList[ParseFailure], List[WalletId]]
  )(
      f: (Period, CurrencyCode, Option[List[WalletId]]) => F[Response[F]]
  ): F[Response[F]] =
    walletsOf(w).fold(
      bad,
      ws =>
        period(from, to)((a, b) =>
          if (a.isAfter(b)) bad("from must not be after to") else currency(c)(ccy => f(Period(a, b), ccy, ws))
        )
    )

  /** Plain text, not a JSON string: the circe codec in scope would otherwise quote it. */
  private def csv(body: String): F[Response[F]] =
    Async[F].pure(
      Response[F](Status.Ok).withEntity(body)(using EntityEncoder.stringEncoder[F]).withContentType(`Content-Type`(MediaType.text.csv))
    )

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- wallets & sharing ----

    case req @ GET -> Root / "budget" / "wallets" =>
      authed(req)(u => money.wallets(u).flatMap(Ok(_)))

    case req @ POST -> Root / "budget" / "wallets" =>
      authed(req)(u => decoded[MoneyService.NewWallet](req)(b => money.createWallet(u, b).flatMap(Created(_))))

    case req @ POST -> Root / "budget" / "wallets" / WalletIdVar(id) / "archive" =>
      authed(req)(u =>
        decoded[BudgetJson.ArchiveBody](req)(b => money.setWalletArchived(u, id, b.archived).flatMap(_.fold(NotFound())(Ok(_))))
      )

    case req @ POST -> Root / "budget" / "wallets" / WalletIdVar(id) / "shares" =>
      authed(req)(u => decoded[BudgetJson.ShareBody](req)(b => money.shareWallet(u, id, b.userId).flatMap(respond(_)(_ => NoContent()))))

    case req @ DELETE -> Root / "budget" / "wallets" / WalletIdVar(id) / "shares" / UserIdVar(member) =>
      authed(req)(u => money.unshareWallet(u, id, member).flatMap(respond(_)(_ => NoContent())))

    case req @ POST -> Root / "budget" / "wallets" / WalletIdVar(id) / "import" :? DryRunParam(dry) =>
      authed(req)(u =>
        req.bodyText.compile.string.flatMap(body => money.importCsv(u, id, body, dry.getOrElse(false)).flatMap(respond(_)(Ok(_))))
      )

    // ---- catalogue ----

    case req @ GET -> Root / "budget" / "categories" =>
      authed(req)(u => money.categories(u).flatMap(Ok(_)))

    case req @ POST -> Root / "budget" / "categories" =>
      authed(req)(u => decoded[MoneyService.NewCategory](req)(b => money.createCategory(u, b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "budget" / "categories" / CategoryIdVar(id) / "archive" =>
      authed(req)(u =>
        decoded[BudgetJson.ArchiveBody](req)(b => money.setCategoryArchived(u, id, b.archived).flatMap(_.fold(NotFound())(Ok(_))))
      )

    case req @ GET -> Root / "budget" / "payees" =>
      authed(req)(u => money.payees(u).flatMap(Ok(_)))

    case req @ POST -> Root / "budget" / "payees" =>
      authed(req)(u => decoded[MoneyService.NewPayee](req)(b => money.createPayee(u, b).flatMap(respond(_)(Created(_)))))

    case req @ GET -> Root / "budget" / "tags" =>
      authed(req)(u => money.tags(u).flatMap(Ok(_)))

    case req @ POST -> Root / "budget" / "tags" =>
      authed(req)(u => decoded[BudgetJson.TagBody](req)(b => money.createTag(u, b.name).flatMap(Created(_))))

    case req @ PUT -> Root / "budget" / "fx-rates" =>
      authed(req)(_ => decoded[List[FxQuote]](req)(rs => money.addRates(rs).flatMap(respond(_)(n => Ok(Map("saved" -> n).asJson)))))

    // ---- transactions ----

    case req @ GET -> Root / "budget" / "transactions" :?
        WalletParam(w) +& CategoryParam(c) +& SearchParam(q) +& From(from) +& To(to) +& Limit(l) =>
      authed(req)(u =>
        (walletsOf(w), optParam(c)).tupled.fold(
          bad,
          { case (ws, cat) =>
            period(from, to)((a, b) =>
              money.transactions(u, MoneyService.Filter(ws.flatMap(_.headOption), cat, q, a, b, limit(l))).flatMap(Ok(_))
            )
          }
        )
      )

    case req @ POST -> Root / "budget" / "transactions" =>
      authed(req)(u => decoded[MoneyService.NewTransaction](req)(b => money.addTransaction(u, b).flatMap(respond(_)(Created(_)))))

    case req @ PUT -> Root / "budget" / "transactions" / TransactionIdVar(id) =>
      authed(req)(u => decoded[MoneyService.NewTransaction](req)(b => money.updateTransaction(u, id, b).flatMap(respond(_)(Ok(_)))))

    case req @ DELETE -> Root / "budget" / "transactions" / TransactionIdVar(id) =>
      authed(req)(u => money.deleteTransaction(u, id).flatMap(respond(_)(_ => NoContent())))

    case req @ GET -> Root / "budget" / "export" :? From(from) +& To(to) =>
      authed(req)(u => period(from, to)((a, b) => money.exportCsv(u, a, b).flatMap(csv)))

    // ---- rules ----

    case req @ GET -> Root / "budget" / "rules" =>
      authed(req)(u => money.rules(u).flatMap(Ok(_)))

    case req @ POST -> Root / "budget" / "rules" =>
      authed(req)(u => decoded[MoneyService.NewRule](req)(b => money.createRule(u, b).flatMap(respond(_)(Created(_)))))

    case req @ DELETE -> Root / "budget" / "rules" / RuleIdVar(id) =>
      authed(req)(u => money.deleteRule(u, id).flatMap(respond(_)(_ => NoContent())))

    case req @ POST -> Root / "budget" / "rules" / RuleIdVar(id) / "run" :? ApplyParam(apply) =>
      authed(req)(u => money.runRuleOnHistory(u, id, apply.getOrElse(false)).flatMap(respond(_)(Ok(_))))

    // ---- budgets ----

    case req @ GET -> Root / "budget" / "budgets" :? AsOf(d) =>
      authed(req)(u => asOf(d)(day => plans.budgets(u, day).flatMap(respond(_)(Ok(_)))))

    case req @ POST -> Root / "budget" / "budgets" =>
      authed(req)(u => decoded[PlanService.NewBudget](req)(b => plans.createBudget(u, b).flatMap(respond(_)(Created(_)))))

    case req @ DELETE -> Root / "budget" / "budgets" / BudgetIdVar(id) =>
      authed(req)(u => plans.deleteBudget(u, id).flatMap(respond(_)(_ => NoContent())))

    case req @ GET -> Root / "budget" / "budgets" / BudgetIdVar(id) / "history" :? AsOf(d) +& PeriodsParam(n) =>
      authed(req)(u => asOf(d)(day => plans.budgetHistory(u, id, day, n.getOrElse(6)).flatMap(respond(_)(Ok(_)))))

    // ---- schedules ----

    case req @ GET -> Root / "budget" / "schedules" =>
      authed(req)(u => plans.schedules(u).flatMap(Ok(_)))

    case req @ POST -> Root / "budget" / "schedules" =>
      authed(req)(u => decoded[PlanService.NewSchedule](req)(b => plans.createSchedule(u, b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "budget" / "schedules" / "process" :? AsOf(d) =>
      authed(req)(u => asOf(d)(day => plans.processDue(u, day).flatMap(n => Ok(Map("recorded" -> n).asJson))))

    case req @ POST -> Root / "budget" / "schedules" / ScheduleIdVar(id) / "active" =>
      authed(req)(u =>
        decoded[BudgetJson.ActiveBody](req)(b => plans.setScheduleActive(u, id, b.active).flatMap(_.fold(NotFound())(Ok(_))))
      )

    case req @ POST -> Root / "budget" / "schedules" / ScheduleIdVar(id) / "pay" =>
      authed(req)(u => decoded[BudgetJson.PayBody](req)(b => plans.pay(u, id, b.date, b.amountMinor).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "budget" / "schedules" / ScheduleIdVar(id) / "skip" =>
      authed(req)(u => plans.skip(u, id).flatMap(respond(_)(next => Ok(Map("nextDue" -> next).asJson))))

    case req @ GET -> Root / "budget" / "upcoming" :? DaysParam(days) =>
      authed(req)(u => today.flatMap(t => plans.upcoming(u, t, days.getOrElse(30).max(0).min(366)).flatMap(Ok(_))))

    case req @ GET -> Root / "budget" / "subscriptions" / "cost" :? CurrencyParam(c) =>
      authed(req)(u => currency(c)(ccy => today.flatMap(t => plans.subscriptionCost(u, ccy, t).flatMap(respond(_)(Ok(_))))))

    // ---- objectives ----

    case req @ GET -> Root / "budget" / "objectives" =>
      authed(req)(u => plans.objectives(u).flatMap(respond(_)(Ok(_))))

    case req @ POST -> Root / "budget" / "objectives" =>
      authed(req)(u => decoded[PlanService.NewObjective](req)(b => plans.createObjective(u, b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "budget" / "objectives" / ObjectiveIdVar(id) / "archive" =>
      authed(req)(u =>
        decoded[BudgetJson.ArchiveBody](req)(b => plans.setObjectiveArchived(u, id, b.archived).flatMap(_.fold(NotFound())(Ok(_))))
      )

    // ---- insights ----

    case req @ GET -> Root / "budget" / "insights" / "summary" :? From(from) +& To(to) +& CurrencyParam(c) +& WalletParam(w) =>
      authed(req)(u => scoped(from, to, c, w)((p, ccy, ws) => insights.summary(u, p, ccy, ws).flatMap(respond(_)(Ok(_)))))

    case req @ GET -> Root / "budget" / "insights" / "categories" :?
        From(from) +& To(to) +& CurrencyParam(c) +& WalletParam(w) +& KindParam(k) =>
      authed(req)(u =>
        param(k, CategoryKind.Expense).fold(
          bad,
          kind =>
            scoped(from, to, c, w)((p, ccy, ws) => insights.categories(u, p, kind, ccy, ws).flatMap(respond(_)(Ok(_))))
        )
      )

    case req @ GET -> Root / "budget" / "insights" / "trend" :?
        From(from) +& To(to) +& CurrencyParam(c) +& WalletParam(w) +& BucketParam(bk) =>
      authed(req)(u =>
        param(bk, Bucket.Month).fold(
          bad,
          bucket =>
            scoped(from, to, c, w)((p, ccy, ws) => insights.trend(u, p, bucket, ccy, ws).flatMap(respond(_)(Ok(_))))
        )
      )

    case req @ GET -> Root / "budget" / "insights" / "compare" :? From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u =>
        scoped(from, to, c, cats.data.Validated.valid(Nil))((p, ccy, _) => insights.compare(u, p, ccy).flatMap(respond(_)(Ok(_))))
      )

    case req @ GET -> Root / "budget" / "insights" / "cashflow" :? From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u =>
        scoped(from, to, c, cats.data.Validated.valid(Nil))((p, ccy, _) => insights.cashflow(u, p, ccy).flatMap(respond(_)(Ok(_))))
      )

    case req @ GET -> Root / "budget" / "insights" / "balances" :? AsOf(d) +& CurrencyParam(c) =>
      authed(req)(u => asOf(d)(day => currency(c)(ccy => insights.balances(u, day, ccy).flatMap(respond(_)(Ok(_))))))

    case req @ GET -> Root / "budget" / "insights" / "balance-history" :? From(from) +& To(to) +& CurrencyParam(c) =>
      authed(req)(u =>
        scoped(from, to, c, cats.data.Validated.valid(Nil))((p, ccy, _) => insights.balanceHistory(u, p, ccy).flatMap(respond(_)(Ok(_))))
      )

    case req @ GET -> Root / "budget" / "insights" / "streak" =>
      authed(req)(u => today.flatMap(t => insights.streak(u, t).flatMap(Ok(_))))

    case req @ GET -> Root / "budget" / "insights" / "recap" :? MonthParam(m) +& CurrencyParam(c) =>
      authed(req)(u =>
        today.flatMap(t =>
          param(m, YearMonth.from(t)).fold(
            bad,
            month =>
              currency(c)(ccy => insights.recap(u, month, ccy).flatMap(respond(_)(Ok(_))))
          )
        )
      )
  }
}
