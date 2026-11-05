package moneystack.auth

import cats.effect.*
import munit.CatsEffectSuite

class PasswordsSuite extends CatsEffectSuite {

  test("passwords shorter than the minimum or longer than bcrypt reads are refused") {
    assert(Passwords.validate("short").isLeft)
    assert(Passwords.validate("x" * Passwords.MinLength).isRight)
    assert(Passwords.validate("é" * 37).isLeft, "74 bytes in UTF-8")
  }

  test("a hash verifies its password and no other") {
    Passwords.hash[IO]("correct horse battery").flatMap { h =>
      assertIO(Passwords.verify[IO]("correct horse battery", Some(h)), true) >>
        assertIO(Passwords.verify[IO]("wrong horse battery", Some(h)), false)
    }
  }

  test("an unknown user never verifies, even with the dummy hash's password") {
    assertIO(Passwords.verify[IO]("dummy password", None), false)
  }
}
