package moneystack.http

import moneystack.config.HttpConfig

import cats.data.Kleisli
import cats.effect.*
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.server.middleware.{CORS, EntityLimiter, Logger, RequestId, Timeout}
import org.typelevel.ci.*
import org.typelevel.log4cats.Logger as Log

import scala.util.control.NonFatal

/**
 * The middleware every request passes through, outermost first: a request id, access logging,
 * error rendering, security headers, CORS when configured, then the timeout and the body-size limit.
 */
object Middleware {

  def apply[F[_]: Async: UUIDGen](cfg: HttpConfig, log: Log[F])(app: HttpApp[F]): HttpApp[F] = {
    val limited  = bodyLimit(cfg.maxBodyBytes)(app)
    val timed    = Timeout.httpApp(cfg.requestTimeout)(limited)
    val shared   = if (cfg.corsOrigins.isEmpty) timed else cors(cfg.corsOrigins)(timed)
    val secured  = securityHeaders(shared)
    val rendered = errors(log)(secured)
    // Authorization and Cookie are redacted by default; bodies are never logged.
    val logged = Logger.httpApp(logHeaders = true, logBody = false)(rendered)
    RequestId.httpApp(logged)
  }

  /** The id `RequestId` assigned, as echoed in the `X-Request-ID` response header. */
  def requestId[F[_]](req: Request[F]): String =
    req.attributes.lookup(RequestId.requestIdAttrKey).getOrElse("-")

  /** 413 for bodies over `max`: at once when `Content-Length` says so, else when streaming passes it. */
  def bodyLimit[F[_]: Async](max: Long)(app: HttpApp[F]): HttpApp[F] = {
    val limited = EntityLimiter.httpApp(app, max)
    Kleisli { req =>
      if (req.contentLength.exists(_ > max)) tooLarge[F](max)
      else limited.run(req)
    }
  }

  private def tooLarge[F[_]: Async](max: Long): F[Response[F]] =
    Async[F].pure(Response[F](Status.PayloadTooLarge).withEntity(Map("error" -> s"request body exceeds $max bytes").asJson))

  /**
   * Turns failures into JSON responses. Malformed requests keep http4s's 400/422 status; anything else is a 500
   * whose body carries only the request id, so internals never leak and the log line can be found.
   */
  def errors[F[_]: Async](log: Log[F])(app: HttpApp[F]): HttpApp[F] =
    Kleisli { req =>
      app.run(req).handleErrorWith {
        case EntityLimiter.EntityTooLarge(max) => tooLarge[F](max)
        // Decoders report a body cut off by the limit as malformed; it is still a body too large.
        case mf: MessageFailure if mf.cause.exists(_.isInstanceOf[EntityLimiter.EntityTooLarge]) =>
          tooLarge[F](mf.cause.collect { case EntityLimiter.EntityTooLarge(max) => max }.getOrElse(0L))
        case mf: MessageFailure =>
          Async[F].pure(Response[F](mf.toHttpResponse[F](req.httpVersion).status).withEntity(Map("error" -> mf.message).asJson))
        case NonFatal(e) =>
          val id = requestId(req)
          log.error(e)(s"Unhandled error in ${req.method} ${req.uri.path} (request $id)").as(
            Response[F](Status.InternalServerError).withEntity(Map("error" -> "internal error", "requestId" -> id).asJson)
          )
      }
    }

  private val SecurityHeaders = Headers(
    "X-Content-Type-Options"  -> "nosniff",
    "X-Frame-Options"         -> "DENY",
    "Referrer-Policy"         -> "no-referrer",
    "Content-Security-Policy" -> "default-src 'none'; frame-ancestors 'none'"
  )

  /** Headers for a JSON API: never sniffed, framed or cached unless a route says otherwise. */
  def securityHeaders[F[_]: Async](app: HttpApp[F]): HttpApp[F] =
    app.map { resp =>
      val withCache = if (resp.headers.contains[headers.`Cache-Control`]) resp else resp.putHeaders("Cache-Control" -> "no-store")
      withCache.putHeaders(SecurityHeaders)
    }

  /** Browsers on `origins` may call the API. Credentials travel in the Authorization header, not cookies. */
  def cors[F[_]: Async](origins: List[String])(app: HttpApp[F]): HttpApp[F] = {
    val allowed = origins.map(CIString(_)).toSet
    CORS.policy
      .withAllowOriginHostCi(allowed.contains)
      .withAllowCredentials(false)
      .withAllowMethodsIn(Set(Method.GET, Method.POST, Method.PUT, Method.PATCH, Method.DELETE))
      .withAllowHeadersIn(Set(ci"Authorization", ci"Content-Type"))
      .withExposeHeadersIn(Set(ci"X-Request-ID"))
      .httpApp(app)
  }
}
