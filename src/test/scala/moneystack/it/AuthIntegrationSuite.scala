package moneystack.it

import moneystack.{App, Main}
import moneystack.config.*
import moneystack.domain.CurrencyCode
import moneystack.http.Middleware

import cats.effect.*
import ciris.Secret
import com.comcast.ip4s.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import org.typelevel.ci.*
import org.typelevel.log4cats.noop.NoOpLogger
import skunk.Session
import skunk.implicits.*

import java.util.UUID
import scala.concurrent.duration.*

/** The whole app over one migrated database, called through its middleware as a client would. */
class AuthIntegrationSuite extends moneystack.testkit.PostgresSuite[HttpApp[IO]] {

  private val config = AppConfig(
    db = DbConfig("localhost", 5432, "test", None, "test", 4, migrateOnStart = false, DbSsl.Disabled, 30.seconds, 1.minute),
    http = HttpConfig(host"0.0.0.0", port"8080", 30.seconds, 1.minute, 1.second, 1024L * 1024, Nil, Duration.Zero),
    auth = AuthConfig(Secret("integration-test-secret-of-32-bytes!"), "moneystack", 1.hour),
    telemetry = TelemetryConfig(enabled = false),
    ledger = LedgerConfig(CurrencyCode.USD)
  )

  def migrations: List[String] = Main.migrations

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, HttpApp[IO]] =
    App.routes(config, pool, NoOpLogger[IO]).map(r => Middleware[IO](config.http, NoOpLogger[IO])(r.orNotFound))

  // ---- client helpers ----

  private def call(method: Method, uri: Uri, token: Option[String] = None, body: Option[Json] = None): IO[(Status, Json)] = {
    val base     = Request[IO](method, uri)
    val withBody = body.fold(base)(base.withEntity(_))
    val req      = token.fold(withBody)(t => withBody.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, t))))
    env.run(req).flatMap(r => r.bodyText.compile.string.map(s => r.status -> io.circe.parser.parse(s).getOrElse(Json.Null)))
  }

  private def freshEmail: String = s"user-${UUID.randomUUID()}@example.com"

  private def signupBody(email: String, password: String = "correct horse battery"): Json =
    Json.obj("email" -> email.asJson, "password" -> password.asJson, "displayName" -> "Test".asJson, "locale" -> "en".asJson)

  /** Signs up a new user, returning their token and id. */
  private def signup(email: String = freshEmail): IO[(String, String)] =
    call(Method.POST, uri"/auth/signup", body = Some(signupBody(email))).map { (status, json) =>
      assertEquals(status, Status.Created, json.noSpaces)
      (json.hcursor.get[String]("token").toOption.get, json.hcursor.downField("user").get[String]("id").toOption.get)
    }

  private def promote(userId: String, role: String): IO[Unit] =
    sessions.use(_.execute(sql"UPDATE users SET role = '#$role' WHERE id = '#$userId'".command)).void

  private def str(json: Json, path: String*): Option[String] =
    path.init.foldLeft(json.hcursor: io.circe.ACursor)(_.downField(_)).get[String](path.last).toOption

  // ---- signup and login ----

  test("signup issues a bearer token, and every new account is an investor whatever it asks for") {
    val body = signupBody(freshEmail).deepMerge(Json.obj("role" -> "admin".asJson))
    call(Method.POST, uri"/auth/signup", body = Some(body)).map { (status, json) =>
      assertEquals(status, Status.Created)
      assertEquals(str(json, "tokenType"), Some("Bearer"))
      assertEquals(str(json, "user", "role"), Some("investor"))
      assert(json.hcursor.downField("user").downField("passwordHash").failed, "never serialise the hash")
    }
  }

  test("a second signup for an email is 409") {
    val email = freshEmail
    signup(email) >> assertIO(call(Method.POST, uri"/auth/signup", body = Some(signupBody(email))).map(_._1), Status.Conflict)
  }

  test("a short password is refused") {
    assertIO(call(Method.POST, uri"/auth/signup", body = Some(signupBody(freshEmail, "short"))).map(_._1), Status.BadRequest)
  }

  test("login answers a token for the right password and the same 401 otherwise") {
    val email                        = freshEmail
    def login(e: String, pw: String) =
      call(Method.POST, uri"/auth/login", body = Some(Json.obj("email" -> e.asJson, "password" -> pw.asJson)))
    for {
      _       <- signup(email)
      good    <- login(email, "correct horse battery")
      wrong   <- login(email, "wrong horse battery!")
      unknown <- login(freshEmail, "correct horse battery")
    } yield {
      assertEquals(good._1, Status.Ok)
      assert(str(good._2, "token").isDefined)
      assertEquals(wrong, unknown, "a wrong password and an unknown email must look alike")
      assertEquals(wrong._1, Status.Unauthorized)
    }
  }

  // ---- tokens ----

  test("no token, or a forged one, is 401 with a Bearer challenge") {
    for {
      none   <- env.run(Request[IO](Method.GET, uri"/auth/me"))
      forged <- call(Method.GET, uri"/auth/me", token = Some("eyJhbGciOiJub25lIn0.eyJzdWIiOiJ4In0."))
    } yield {
      assertEquals(none.status, Status.Unauthorized)
      assert(none.headers.get(ci"WWW-Authenticate").exists(_.head.value.startsWith("Bearer")))
      assertEquals(forged._1, Status.Unauthorized)
    }
  }

  test("the token identifies its user") {
    signup().flatMap { (token, id) =>
      assertIO(call(Method.GET, uri"/auth/me", token = Some(token)).map(r => str(r._2, "id")), Some(id))
    }
  }

  test("feature modules refuse a missing token with 401 too") {
    assertIO(call(Method.GET, uri"/budget/wallets").map(_._1), Status.Unauthorized) >>
      signup().flatMap(t => assertIO(call(Method.GET, uri"/budget/wallets", token = Some(t._1)).map(_._1), Status.Ok))
  }

  // ---- roles and ownership ----

  test("an investor may not use admin routes") {
    signup().flatMap { (token, _) =>
      val plan = Json.obj(
        "code"       -> "x".asJson,
        "name"       -> "X".asJson,
        "priceMinor" -> 100.asJson,
        "currency"   -> "USD".asJson,
        "interval"   -> "monthly".asJson,
        "active"     -> true.asJson
      )
      assertIO(call(Method.POST, uri"/admin/plans", token = Some(token), body = Some(plan)).map(_._1), Status.Forbidden)
    }
  }

  test("an admin changes a role, and it applies to tokens already issued") {
    for {
      (adminToken, adminId) <- signup()
      _                     <- promote(adminId, "admin")
      (userToken, userId)   <- signup()
      before                <- call(Method.GET, uri"/accountant/users" / userId / "payments", token = Some(userToken))
      changed               <- call(
        Method.PUT,
        uri"/admin/users" / userId / "role",
        token = Some(adminToken),
        body = Some(Json.obj("role" -> "accountant".asJson))
      )
      after <- call(Method.GET, uri"/accountant/users" / userId / "payments", token = Some(userToken))
    } yield {
      assertEquals(before._1, Status.Forbidden)
      assertEquals(str(changed._2, "role"), Some("accountant"))
      assertEquals(after._1, Status.Ok)
    }
  }

  test("an investor cannot see or change another investor's portfolio") {
    val portfolio = Json.obj("name" -> "Mine".asJson, "baseCurrency" -> "USD".asJson)
    val holding   = Json.obj("ticker" -> "AAPL".asJson, "exchange" -> "NASDAQ".asJson, "quantity" -> 1.asJson, "avgCostUnit" -> 1.asJson)
    for {
      (owner, _) <- signup()
      (other, _) <- signup()
      created    <- call(Method.POST, uri"/investor/portfolios", token = Some(owner), body = Some(portfolio))
      pid = str(created._2, "id").get
      mine   <- call(Method.GET, uri"/investor/portfolios" / pid / "valuation", token = Some(owner))
      theirs <- call(Method.GET, uri"/investor/portfolios" / pid / "valuation", token = Some(other))
      write  <- call(Method.POST, uri"/investor/portfolios" / pid / "holdings", token = Some(other), body = Some(holding))
    } yield {
      assertEquals(created._1, Status.Created)
      assertEquals(mine._1, Status.Ok)
      assertEquals(theirs._1, Status.NotFound)
      assertEquals(write._1, Status.NotFound)
    }
  }

  // ---- revocation ----

  private def login(email: String, password: String) =
    call(Method.POST, uri"/auth/login", body = Some(Json.obj("email" -> email.asJson, "password" -> password.asJson)))

  test("changing the password revokes earlier tokens and answers a working one") {
    val email = freshEmail
    val body  = Json.obj("currentPassword" -> "correct horse battery".asJson, "newPassword" -> "a new horse battery".asJson)
    for {
      (old, _) <- signup(email)
      wrong    <-
        call(Method.POST, uri"/auth/password", token = Some(old), body = Some(body.deepMerge(Json.obj("currentPassword" -> "nope".asJson))))
      changed  <- call(Method.POST, uri"/auth/password", token = Some(old), body = Some(body))
      oldNow   <- call(Method.GET, uri"/auth/me", token = Some(old))
      fresh    <- call(Method.GET, uri"/auth/me", token = str(changed._2, "token"))
      newLogin <- login(email, "a new horse battery")
    } yield {
      assertEquals(wrong._1, Status.Forbidden)
      assertEquals(changed._1, Status.Ok)
      assertEquals(oldNow._1, Status.Unauthorized)
      assertEquals(fresh._1, Status.Ok)
      assertEquals(newLogin._1, Status.Ok)
    }
  }

  test("signing out everywhere revokes every token the user holds") {
    val email = freshEmail
    for {
      (first, _) <- signup(email)
      second     <- login(email, "correct horse battery").map(r => str(r._2, "token").get)
      out        <- call(Method.POST, uri"/auth/logout-all", token = Some(second))
      a          <- call(Method.GET, uri"/auth/me", token = Some(first))
      b          <- call(Method.GET, uri"/auth/me", token = Some(second))
    } yield {
      assertEquals(out._1, Status.NoContent)
      assertEquals((a._1, b._1), (Status.Unauthorized, Status.Unauthorized))
    }
  }
}
