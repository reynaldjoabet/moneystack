package moneystack.http

import moneystack.auth.{Passwords, Tokens}
import moneystack.domain.*
import moneystack.db.*
import moneystack.service.*
import moneystack.external.MarketData
import moneystack.platform.{AuthFailure, HttpSupport}

import cats.effect.*
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import skunk.SqlState

import java.time.{Instant, LocalDate, ZoneOffset}
import java.util.UUID

/**
 * Role-aware routes. The caller is the user named by the bearer token (see [[Principal]]);
 * signup and login are the only routes that issue one. Portfolios are visible to their owner only.
 */
final class Routes[F[_]: Async](
    principal: Principal[F],
    tokens: Tokens[F],
    passwordResets: PasswordResetService[F],
    audit: AuditLog[F],
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

  private object PortfolioIdVar {
    def unapply(s: String): Option[PortfolioId] =
      scala.util.Try(UUID.fromString(s)).toOption.map(PortfolioId.assume)
  }

  private object ModelIdVar {
    def unapply(s: String): Option[ModelPortfolioId] =
      scala.util.Try(UUID.fromString(s)).toOption.map(ModelPortfolioId.assume)
  }

  private object UserIdVar {
    def unapply(s: String): Option[UserId] =
      scala.util.Try(UUID.fromString(s)).toOption.map(UserId.assume)
  }

  /** The caller when they hold `role`; otherwise the 401 or 403 to answer with. */
  private def requireRole(req: Request[F], role: Role): F[Either[Response[F], User]] =
    principal.user(req).flatMap {
      case Right(u) if u.role == role        => Async[F].pure(Right(u))
      case Right(_)                          => forbidden.map(Left(_))
      case Left(AuthFailure.Forbidden)       => forbidden.map(Left(_))
      case Left(AuthFailure.Unauthenticated) => HttpSupport.unauthorized[F].map(Left(_))
    }

  private def forbidden: F[Response[F]] = Forbidden(Map("error" -> "forbidden").asJson)

  /** Runs `f` on the caller's own portfolio. Someone else's answers 404, as if it did not exist. */
  private def ownPortfolio(owner: User, pid: PortfolioId)(f: Portfolio => F[Response[F]]): F[Response[F]] =
    portfolios.find(pid).flatMap {
      case Some(p) if p.userId == owner.id => f(p)
      case _                               => NotFound(Map("error" -> "portfolio not found").asJson)
    }

  private def origin(req: Request[F]): AuditLog.Origin =
    AuditLog.Origin(Middleware.requestId(req), req.headers.get(org.typelevel.ci.CIString("X-Forwarded-For")).map(_.head.value))

  private def session(u: User): F[Json.AuthSession] =
    tokens.issue(u.id, u.tokenVersion).map(t => Json.AuthSession(t.token, t.expiresAt, u))

  /** Runs `f` for any signed-in caller, 401 otherwise. */
  private def signedIn(req: Request[F])(f: User => F[Response[F]]): F[Response[F]] =
    principal.user(req).flatMap(_.fold(_ => HttpSupport.unauthorized[F], f))

  private val emailTaken: F[Response[F]] = Conflict(Map("error" -> "email already registered").asJson)

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- auth ----

    case req @ POST -> Root / "auth" / "signup" =>
      req.as[Json.SignupBody].flatMap { b =>
        Passwords.validate(b.password) match {
          case Left(err) => BadRequest(Map("error" -> err).asJson)
          case Right(pw) =>
            users.findByEmail(b.email).flatMap {
              case Some(_) => emailTaken
              case None    =>
                for {
                  hash <- Passwords.hash[F](pw)
                  now  <- Async[F].realTimeInstant
                  u = User(
                    id = UserId.assume(UUID.randomUUID()),
                    email = b.email,
                    passwordHash = hash,
                    displayName = b.displayName,
                    role = Role.Investor,
                    locale = b.locale,
                    telegramChatId = None,
                    createdAt = now
                  )
                  resp <- users.create(u)
                    .flatMap(saved =>
                      audit.record(AuditLog.Signup, origin(req), Some(saved.id), Some(saved.id)) *> session(saved).flatMap(Created(_))
                    )
                    // Two signups for one email can both pass the lookup; the unique index decides.
                    .recoverWith { case SqlState.UniqueViolation(_) => emailTaken }
                } yield resp
            }
        }
      }

    case req @ POST -> Root / "auth" / "login" =>
      req.as[Json.LoginBody].flatMap { b =>
        for {
          found <- users.findByEmail(b.email)
          ok    <- Passwords.verify[F](b.password, found.map(_.passwordHash))
          _     <- audit.record(if (ok) AuditLog.LoginSucceeded else AuditLog.LoginFailed, origin(req), None, found.map(_.id))
          resp  <- found.filter(_ => ok) match {
            case Some(u) => session(u).flatMap(Ok(_))
            case None    => Unauthorized(
                headers.`WWW-Authenticate`(Challenge("Bearer", "moneystack")),
                Map("error" -> "invalid email or password").asJson
              )
          }
        } yield resp
      }

    case req @ GET -> Root / "auth" / "me" =>
      signedIn(req)(u => Ok(Json.userEncoder(u)))

    // Revokes every token issued so far, this one included, and answers a fresh one for this client.
    case req @ POST -> Root / "auth" / "password" =>
      signedIn(req) { u =>
        req.as[Json.ChangePasswordBody].flatMap { b =>
          Passwords.verify[F](b.currentPassword, Some(u.passwordHash)).flatMap {
            case false =>
              audit.record(AuditLog.PasswordChangeRefused, origin(req), Some(u.id), Some(u.id)) *>
                Forbidden(Map("error" -> "current password is wrong").asJson)
            case true =>
              Passwords.validate(b.newPassword) match {
                case Left(err) => BadRequest(Map("error" -> err).asJson)
                case Right(pw) =>
                  Passwords.hash[F](pw).flatMap(users.setPassword(u.id, _)).flatMap {
                    case Some(updated) =>
                      audit.record(AuditLog.PasswordChanged, origin(req), Some(u.id), Some(u.id)) *> session(updated).flatMap(Ok(_))
                    case None => HttpSupport.unauthorized[F]
                  }
              }
          }
        }
      }

    // Always 202, whether or not the email has an account.
    case req @ POST -> Root / "auth" / "password" / "forgot" =>
      req.as[Json.ForgotPasswordBody].flatMap(b => passwordResets.request(b.email, origin(req)) *> Accepted())

    // Trades a reset token for a new password. Like a password change, it revokes every earlier token.
    case req @ POST -> Root / "auth" / "password" / "reset" =>
      req.as[Json.ResetPasswordBody].flatMap { b =>
        passwordResets.reset(b.token, b.newPassword).flatMap {
          case Left(err) =>
            audit.record(AuditLog.PasswordResetRefused, origin(req), None, None) *> BadRequest(Map("error" -> err).asJson)
          case Right(u) => audit.record(AuditLog.PasswordReset, origin(req), None, Some(u.id)) *> session(u).flatMap(Ok(_))
        }
      }

    // Signs the user out on every device: all their tokens stop working.
    case req @ POST -> Root / "auth" / "logout-all" =>
      signedIn(req)(u =>
        users.revokeTokens(u.id) *> audit.record(AuditLog.TokensRevoked, origin(req), Some(u.id), Some(u.id)) *> NoContent()
      )

    // ---- investor: portfolios + holdings + valuations ----

    case req @ POST -> Root / "investor" / "portfolios" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) =>
          req.as[Json.CreatePortfolioBody].flatMap { b =>
            for {
              now <- Async[F].delay(Instant.now())
              p = Portfolio(PortfolioId.assume(UUID.randomUUID()), u.id, b.name, b.baseCurrency, b.backedBy, now)
              s <- portfolios.createPortfolio(p)
              r <- Created(s)
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
        case Right(u) =>
          ownPortfolio(u, pid) { _ =>
            req.as[Json.UpsertHoldingBody].flatMap { b =>
              for {
                now <- Async[F].delay(Instant.now())
                h = Holding(HoldingId.assume(UUID.randomUUID()), pid, b.ticker, b.exchange, b.quantity, b.avgCostUnit, now)
                s <- holdings.upsert(h)
                r <- Created(s)
              } yield r
            }
          }
      }

    case req @ GET -> Root / "investor" / "portfolios" / PortfolioIdVar(pid) / "valuation" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) => ownPortfolio(u, pid)(p => portfolioSvc.value(p).flatMap(Ok(_)))
      }

    case req @ GET -> Root / "investor" / "portfolios" / PortfolioIdVar(pid) / "performance" =>
      requireRole(req, Role.Investor).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(u) =>
          ownPortfolio(u, pid)(p =>
            Async[F].delay(LocalDate.now(ZoneOffset.UTC)).flatMap(today => portfolioSvc.performance(p, today)).flatMap(Ok(_))
          )
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
              mp = ModelPortfolio(
                ModelPortfolioId.assume(UUID.randomUUID()),
                b.name,
                b.strategy,
                u.id,
                now,
                now,
                b.description
              )
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
            recommendationSvc.publish(
              u.id,
              b.modelId,
              b.ticker,
              b.exchange,
              b.action,
              b.rationale,
              b.targetWeightBps
            ).flatMap(Created(_))
          )
      }

    case GET -> Root / "recommendations" :? RecLimit(limit) =>
      recommendations.listLatest(limit.getOrElse(20)).flatMap(Ok(_))

    // ---- market data passthrough ----

    case GET -> Root / "quote" / tickerSeg / exchangeSeg =>
      (Ticker.either(tickerSeg), Exchange.either(exchangeSeg)).tupled match {
        case Left(err)     => BadRequest(Map("error" -> err).asJson)
        case Right((t, e)) => market.quote(t, e).flatMap(_.fold(NotFound())(Ok(_)))
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

    // ---- admin: roles (the first admin is promoted in SQL, see the README) ----

    case req @ PUT -> Root / "admin" / "users" / UserIdVar(uid) / "role" =>
      requireRole(req, Role.Admin).flatMap {
        case Left(r)      => Async[F].pure(r)
        case Right(admin) =>
          req.as[Json.SetRoleBody].flatMap(b =>
            users.setRole(uid, b.role).flatMap {
              case None    => NotFound(Map("error" -> "user not found").asJson)
              case Some(u) =>
                audit.record(AuditLog.RoleChanged, origin(req), Some(admin.id), Some(u.id), Some(s"role=${Role.render(b.role)}")) *>
                  Ok(Json.userEncoder(u))
            }
          )
      }

    // ---- admin: audit log, newest first; `user` narrows it to events about one account ----

    case req @ GET -> Root / "admin" / "audit" :? AuditUser(user) +& AuditLimit(limit) =>
      requireRole(req, Role.Admin).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          user.traverse(s => UserIdVar.unapply(s).toRight(s)) match {
            case Left(_)        => BadRequest(Map("error" -> "user must be a UUID").asJson)
            case Right(subject) => audit.recent(subject, limit.getOrElse(100).max(1).min(500)).flatMap(Ok(_))
          }
      }

    // ---- accountant: payment list ----

    case req @ GET -> Root / "accountant" / "users" / userSeg / "payments" =>
      requireRole(req, Role.Accountant).flatMap {
        case Left(r)  => Async[F].pure(r)
        case Right(_) =>
          UserIdVar.unapply(userSeg) match {
            case None      => BadRequest()
            case Some(uid) => subscriptions.listPaymentsFor(uid, 200).flatMap(Ok(_))
          }
      }
  }

  private object AuditUser  extends OptionalQueryParamDecoderMatcher[String]("user")
  private object AuditLimit extends OptionalQueryParamDecoderMatcher[Int]("limit")
  private object RecLimit   extends OptionalQueryParamDecoderMatcher[Int]("limit")
}
