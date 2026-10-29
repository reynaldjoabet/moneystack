package moneystack.http

import moneystack.domain.*
import moneystack.db.*
import moneystack.service.*
import moneystack.external.MarketData

import cats.effect.*
import cats.syntax.all.*
import com.github.t3hnar.bcrypt.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Role-aware routes. The header `X-User-Id` is the trusted principal (in
 * production this is set by a JWT-validating middleware upstream). Each route
 * that mutates portfolio state also re-checks the user's role.
 */
final class Routes[F[_]: Async](
    users: Users[F],
    portfolios: Portfolios[F],
    holdings: Holdings[F],
    recommendations: Recommendations[F],
    subscriptions: Subscriptions[F],
    market: MarketData[F],
    portfolioSvc: PortfolioService[F],
    recommendationSvc: RecommendationService[F],
    subscriptionSvc: SubscriptionService[F]
) extends Http4sDsl[F] {

  import Json.given

  private object UserHeader {
    def unapply(req: Request[F]): Option[UserId] =
      req.headers.get(org.typelevel.ci.CIString("X-User-Id"))
        .flatMap(h => scala.util.Try(UUID.fromString(h.head.value)).toOption.map(UserId.assume))
  }

  private object PortfolioIdVar {
    def unapply(s: String): Option[PortfolioId] =
      scala.util.Try(UUID.fromString(s)).toOption.map(PortfolioId.assume)
  }

  private object ModelIdVar {
    def unapply(s: String): Option[ModelPortfolioId] =
      scala.util.Try(UUID.fromString(s)).toOption.map(ModelPortfolioId.assume)
  }

  private def requireRole(req: Request[F], role: Role): F[Either[Response[F], User]] =
    UserHeader.unapply(req) match {
      case None      => Forbidden().map(Left(_))
      case Some(uid) =>
        users.find(uid).flatMap {
          case Some(u) if u.role == role => Async[F].pure(Right(u))
          case _                          => Forbidden().map(Left(_))
        }
    }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root / "health" =>
      Ok(Map("status" -> "ok").asJson)

    // ---- auth ----

    case req @ POST -> Root / "auth" / "signup" =>
      req.as[Json.SignupBody].flatMap { b =>
        for {
          hash <- Async[F].delay(b.password.bcryptBounded(12))
          now  <- Async[F].delay(Instant.now())
          u     = User(
                    id             = UserId.assume(UUID.randomUUID()),
                    email          = b.email,
                    passwordHash   = PasswordHash.assume(hash),
                    displayName    = b.displayName,
                    role           = b.role,
                    locale         = b.locale,
                    telegramChatId = None,
                    createdAt      = now
                  )
          saved <- users.create(u)
          resp  <- Created(Json.userEncoder(saved))
        } yield resp
      }

    case req @ POST -> Root / "auth" / "login" =>
      req.as[Json.LoginBody].flatMap { b =>
        users.findByEmail(b.email).flatMap {
          case Some(u) if b.password.isBcryptedBounded(u.passwordHash.value) => Ok(Json.userEncoder(u))
          case _                                                              => Forbidden(Map("error" -> "invalid").asJson)
        }
      }

    // ---- investor: portfolios + holdings + valuations ----

    case req @ POST -> Root / "investor" / "portfolios" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) =>
          req.as[Json.CreatePortfolioBody].flatMap { b =>
            for {
              now <- Async[F].delay(Instant.now())
              p    = Portfolio(PortfolioId.assume(UUID.randomUUID()), u.id, b.name, b.baseCurrency, b.backedBy, now)
              s   <- portfolios.createPortfolio(p)
              r   <- Created(s)
            } yield r
          }
      }

    case req @ GET -> Root / "investor" / "portfolios" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) => portfolios.listFor(u.id).flatMap(Ok(_))
      }

    case req @ POST -> Root / "investor" / "portfolios" / PortfolioIdVar(pid) / "holdings" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          req.as[Json.UpsertHoldingBody].flatMap { b =>
            for {
              now <- Async[F].delay(Instant.now())
              h    = Holding(HoldingId.assume(UUID.randomUUID()), pid, b.ticker, b.exchange, b.quantity, b.avgCostUnit, now)
              s   <- holdings.upsert(h)
              r   <- Created(s)
            } yield r
          }
      }

    case req @ GET -> Root / "investor" / "portfolios" / PortfolioIdVar(pid) / "valuation" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          portfolios.find(pid).flatMap {
            case Some(p) => portfolioSvc.value(p).flatMap(Ok(_))
            case None    => NotFound()
          }
      }

    case req @ GET -> Root / "investor" / "portfolios" / PortfolioIdVar(pid) / "performance" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          portfolios.find(pid).flatMap {
            case Some(p) => portfolioSvc.performance(p, LocalDate.now()).flatMap(Ok(_))
            case None    => NotFound()
          }
      }

    // ---- investor: subscription ----

    case GET -> Root / "plans" =>
      subscriptions.listPlans().flatMap(Ok(_))

    case req @ POST -> Root / "investor" / "subscribe" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) =>
          req.as[Json.SubscribeBody].flatMap(b =>
            subscriptionSvc.subscribe(u.id, b.planId).flatMap {
              case Right(s)  => Created(s)
              case Left(err) => BadRequest(Map("error" -> err.toString).asJson)
            }
          )
      }

    // ---- analyst: model portfolios + recommendations ----

    case req @ POST -> Root / "analyst" / "models" =>
      requireRole(req, Role.Analyst).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) =>
          req.as[Json.CreateModelBody].flatMap { b =>
            for {
              now <- Async[F].delay(Instant.now())
              mp   = ModelPortfolio(ModelPortfolioId.assume(UUID.randomUUID()),
                                    b.name, b.strategy, u.id, now, now, b.description)
              res <- portfolioSvc.saveModel(mp, b.allocations)
              r   <- res match {
                       case Right(saved) => Created(saved)
                       case Left(err)    => BadRequest(Map("error" -> err.toString).asJson)
                     }
            } yield r
          }
      }

    case GET -> Root / "models" =>
      portfolios.listModels().flatMap(Ok(_))

    case GET -> Root / "models" / ModelIdVar(mid) / "allocations" =>
      portfolios.modelAllocations(mid).flatMap(Ok(_))

    case req @ POST -> Root / "analyst" / "recommendations" =>
      requireRole(req, Role.Analyst).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) =>
          req.as[Json.PublishRecBody].flatMap(b =>
            recommendationSvc.publish(u.id, b.modelId, b.ticker, b.exchange,
                                       b.action, b.rationale, b.targetWeightBps).flatMap(Created(_))
          )
      }

    case GET -> Root / "recommendations" :? RecLimit(limit) =>
      recommendations.listLatest(limit.getOrElse(20)).flatMap(Ok(_))

    // ---- market data passthrough ----

    case GET -> Root / "quote" / tickerSeg / exchangeSeg =>
      (Ticker.either(tickerSeg), Exchange.either(exchangeSeg)).tupled match {
        case Left(err)        => BadRequest(Map("error" -> err).asJson)
        case Right((t, e))    => market.quote(t, e).flatMap(_.fold(NotFound())(Ok(_)))
      }

    // ---- admin: plan management ----

    case req @ POST -> Root / "admin" / "plans" =>
      requireRole(req, Role.Admin).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          req.as[Json.UpsertPlanBody].flatMap { b =>
            val p = Plan(PlanId.assume(UUID.randomUUID()), b.code, b.name, b.priceMinor, b.currency, b.interval, b.active)
            subscriptions.upsertPlan(p).flatMap(Created(_))
          }
      }

    // ---- accountant: payment list ----

    case req @ GET -> Root / "accountant" / "users" / userSeg / "payments" =>
      requireRole(req, Role.Accountant).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          scala.util.Try(UUID.fromString(userSeg)).toOption.map(UserId.assume) match {
            case None      => BadRequest()
            case Some(uid) => subscriptions.listPaymentsFor(uid, 200).flatMap(Ok(_))
          }
      }
  }

  private object RecLimit extends OptionalQueryParamDecoderMatcher[Int]("limit")
}
