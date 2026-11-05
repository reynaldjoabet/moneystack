package moneystack

import moneystack.config.TelemetryConfig

import cats.effect.*
import cats.effect.unsafe.IORuntime
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.trace.redact.{PathRedactor, QueryRedactor}
import org.http4s.otel4s.middleware.trace.server.{ServerMiddleware, ServerSpanDataProvider}
import org.http4s.server.middleware.Metrics
import org.typelevel.otel4s.instrumentation.ce.IORuntimeMetrics
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.trace.TracerProvider

import scala.jdk.CollectionConverters.*

/** Where traces and metrics go: OTLP when enabled, nowhere otherwise. */
final case class Telemetry(tracerProvider: TracerProvider[IO], meterProvider: MeterProvider[IO])

object Telemetry {

  val noop: Telemetry = Telemetry(TracerProvider.noop[IO], MeterProvider.noop[IO])

  /**
   * An OpenTelemetry SDK configured from the standard `OTEL_*` variables (endpoint, protocol, sampler, ...),
   * plus cats-effect runtime metrics. `otel.service.name` defaults to moneystack; the environment overrides it.
   */
  def make(cfg: TelemetryConfig, runtime: IORuntime): Resource[IO, Telemetry] =
    if (!cfg.enabled) Resource.pure(noop)
    else
      for {
        otel <- OtelJava.autoConfigured[IO](_.addPropertiesSupplier(() => Map("otel.service.name" -> "moneystack").asJava))
        t = Telemetry(otel.tracerProvider, otel.meterProvider)
        _ <- IORuntimeMetrics.register[IO](runtime.metrics, IORuntimeMetrics.Config.default)(using Sync[IO], t.meterProvider)
      } yield t

  private val noRedaction = new PathRedactor.NeverRedact with QueryRedactor.NeverRedact {}

  /** The standard HTTP server metrics (duration, active requests, aborts) for every request. */
  def metrics(t: Telemetry)(routes: HttpRoutes[IO]): IO[HttpRoutes[IO]] = {
    given MeterProvider[IO] = t.meterProvider
    OtelMetrics.serverMetricsOps[IO]().map(ops => Metrics[IO](ops)(routes))
  }

  /** A server span around every request, continuing a trace the caller propagated. */
  def tracing(t: Telemetry)(app: HttpApp[IO]): IO[HttpApp[IO]] = {
    given TracerProvider[IO] = t.tracerProvider
    ServerMiddleware.builder[IO](ServerSpanDataProvider.openTelemetry(noRedaction)).build.map(_.wrapHttpApp(app))
  }
}
