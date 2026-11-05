package moneystack.http

import moneystack.config.HttpConfig

import cats.effect.*
import com.comcast.ip4s.*
import fs2.Stream
import io.circe.Json
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.typelevel.ci.*
import org.typelevel.log4cats.noop.NoOpLogger

import scala.concurrent.duration.*

class MiddlewareSuite extends CatsEffectSuite {

  private val cfg = HttpConfig(
    host = host"0.0.0.0",
    port = port"8080",
    requestTimeout = 200.millis,
    idleTimeout = 1.minute,
    shutdownTimeout = 1.second,
    maxBodyBytes = 16,
    corsOrigins = List("https://app.example.com")
  )

  private val routes = HttpRoutes.of[IO] {
    case GET -> Root / "ok"          => Ok("fine")
    case GET -> Root / "boom"        => IO.raiseError(new IllegalStateException("db password is hunter2"))
    case GET -> Root / "slow"        => IO.sleep(1.second) >> Ok()
    case req @ POST -> Root / "echo" => req.as[String](using summon, EntityDecoder.text[IO]).flatMap(Ok(_))
    case req @ POST -> Root / "json" => req.as[Json].flatMap(Ok(_))
  }

  private val app = Middleware[IO](cfg, NoOpLogger[IO])(routes.orNotFound)

  private def run(req: Request[IO]): IO[Response[IO]] = app.run(req)

  test("every response carries a request id and the security headers") {
    run(Request(GET, uri"/ok")).map { r =>
      assert(r.headers.get(ci"X-Request-ID").isDefined)
      assertEquals(r.headers.get(ci"X-Content-Type-Options").map(_.head.value), Some("nosniff"))
      assertEquals(r.headers.get(ci"X-Frame-Options").map(_.head.value), Some("DENY"))
      assertEquals(r.headers.get(ci"Cache-Control").map(_.head.value), Some("no-store"))
    }
  }

  test("an unhandled error is a 500 naming the request id, never the cause") {
    for {
      r    <- run(Request(GET, uri"/boom"))
      body <- r.as[Json]
    } yield {
      assertEquals(r.status, Status.InternalServerError)
      assertEquals(body.hcursor.get[String]("requestId").toOption, r.headers.get(ci"X-Request-ID").map(_.head.value))
      assert(!body.noSpaces.contains("hunter2"), body.noSpaces)
    }
  }

  test("a malformed JSON body is a client error, not a 500") {
    val req = Request[IO](POST, uri"/json").withEntity("{nope")(using EntityEncoder.stringEncoder[IO]).withContentType(
      headers.`Content-Type`(MediaType.application.json)
    )
    assertIO(run(req).map(_.status.responseClass), Status.ClientError)
  }

  test("a body declared larger than the limit is refused before it is read") {
    assertIO(run(Request[IO](POST, uri"/echo").withEntity("x" * 17)).map(_.status), Status.PayloadTooLarge)
  }

  test("a streamed body that passes the limit is refused") {
    val chunked = Request[IO](POST, uri"/echo").withBodyStream(Stream.emits(("x" * 40).getBytes).chunkLimit(4).unchunks)
    assertIO(run(chunked).map(_.status), Status.PayloadTooLarge)
  }

  test("a request over the timeout is answered 503") {
    assertIO(run(Request(GET, uri"/slow")).map(_.status), Status.ServiceUnavailable)
  }

  test("CORS headers go to listed origins only") {
    def allowOrigin(origin: String) =
      run(Request[IO](GET, uri"/ok").putHeaders("Origin" -> origin)).map(_.headers.get(ci"Access-Control-Allow-Origin").map(_.head.value))
    assertIO(allowOrigin("https://app.example.com"), Some("https://app.example.com")) >>
      assertIO(allowOrigin("https://evil.example.com"), None)
  }
}
