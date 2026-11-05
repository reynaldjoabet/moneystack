package moneystack.auth

import moneystack.domain.UserId

import cats.effect.*
import munit.CatsEffectSuite
import pdi.jwt.{JwtAlgorithm, JwtCirce, JwtClaim}

import java.util.{Base64, UUID}
import scala.concurrent.duration.*

class TokensSuite extends CatsEffectSuite {

  private val secret = "s" * Tokens.MinSecretBytes
  private val tokens = Tokens.make[IO](secret, "moneystack", 1.hour)
  private val user   = UserId.assume(UUID.randomUUID())

  test("an issued token verifies as its user") {
    tokens.issue(user).flatMap(t => assertIO(tokens.verify(t.token), Some(user)))
  }

  test("a token expires after its ttl") {
    Tokens.make[IO](secret, "moneystack", (-1).minute).issue(user).flatMap(t => assertIO(tokens.verify(t.token), None))
  }

  test("a token signed with another key is refused") {
    Tokens.make[IO]("o" * Tokens.MinSecretBytes, "moneystack", 1.hour).issue(user).flatMap(t => assertIO(tokens.verify(t.token), None))
  }

  test("a token from another issuer is refused") {
    Tokens.make[IO](secret, "elsewhere", 1.hour).issue(user).flatMap(t => assertIO(tokens.verify(t.token), None))
  }

  test("a tampered payload is refused") {
    tokens.issue(UserId.assume(UUID.randomUUID())).flatMap { t =>
      val Array(header, _, signature) = t.token.split('.'): @unchecked
      val payload                     = Base64.getUrlEncoder.withoutPadding.encodeToString(
        s"""{"iss":"moneystack","sub":"${user.value}","exp":${Long.MaxValue / 1000}}""".getBytes
      )
      assertIO(tokens.verify(s"$header.$payload.$signature"), None)
    }
  }

  test("an unsigned (alg none) token is refused") {
    val claim = JwtClaim(issuer = Some("moneystack"), subject = Some(user.value.toString), expiration = Some(Long.MaxValue / 1000))
    assertIO(tokens.verify(JwtCirce.encode(claim)), None)
  }

  test("an HS512 token signed with our key is refused: only HS256 is accepted") {
    val claim = JwtClaim(issuer = Some("moneystack"), subject = Some(user.value.toString), expiration = Some(Long.MaxValue / 1000))
    assertIO(tokens.verify(JwtCirce.encode(claim, secret, JwtAlgorithm.HS512)), None)
  }

  test("garbage is refused") {
    assertIO(tokens.verify("not.a.token"), None) >> assertIO(tokens.verify(""), None)
  }
}
