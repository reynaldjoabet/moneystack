package moneystack.budget.domain

import moneystack.domain.*
import Fixtures.*

import io.circe.parser.decode
import io.circe.syntax.*

import java.time.Instant
import java.util.UUID

class RulesSuite extends munit.FunSuite {

  private val w        = wallet()
  private val coffee   = category("Coffee")
  private val tagId    = TagId.assume(UUID.randomUUID())

  private def rule(conds: List[Condition], actions: List[Action], all: Boolean = true, position: Int = 1) =
    Rule(RuleId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("r"), position, enabled = true, all, conds, actions, 0L, Instant.EPOCH)

  private val starbucks = rule(
    List(Condition.Text(TextField.Title, TextOp.ContainsAny, List("starbucks", "costa")), Condition.Amount(AmountOp.Lte, None, Some(2000))),
    List(Action.SetCategory(coffee.id), Action.AddTags(List(tagId)), Action.SetNote(NoteMode.Append, "#coffee"))
  )

  test("all conditions must hold; actions set category, add tags and append notes") {
    val t = tx(w, TxKind.Expense, 450, d(3, 2), title = "STARBUCKS #123")
    val (out, hits) = Rules.run(List(starbucks), t)
    assertEquals(out.categoryId, Some(coffee.id))
    assertEquals(out.categorySource, CategorySource.Rule)
    assertEquals(out.tags, List(tagId))
    assertEquals(out.note.map(n => n.value: String), Some("#coffee"))
    assertEquals(hits, List(starbucks.id))
    assertEquals(Rules.run(List(starbucks), t.copy(amountMinor = 5000))._2, Nil)
  }

  test("a hand-picked category is never overwritten") {
    val manual = tx(w, TxKind.Expense, 450, d(3, 2), cat = Some(category("Treats")), title = "Costa")
    val out = Rules.applyTo(starbucks, manual)
    assertEquals(out.categoryId, manual.categoryId)
    assertEquals(out.tags, List(tagId))                 // other actions still apply
  }

  test("any-of rules, list and day conditions, and disabled rules") {
    val r = rule(List(Condition.Wallet(in = false, List(w.id)), Condition.DayOfMonth(1, 5)), List(Action.AddTags(List(tagId))), all = false)
    assert(Rules.matches(r, tx(w, TxKind.Expense, 1, d(3, 3))))
    assert(!Rules.matches(r, tx(w, TxKind.Expense, 1, d(3, 20))))
    assert(!Rules.matches(r.copy(enabled = false), tx(w, TxKind.Expense, 1, d(3, 3))))
    assert(Rules.matches(rule(List(Condition.Text(TextField.Note, TextOp.IsEmpty, Nil)), List(Action.AddTags(Nil))), tx(w, TxKind.Income, 1, d(3, 3))))
  }

  test("validation") {
    assert(Rules.validate(Nil, List(Action.AddTags(Nil))).isLeft)
    assert(Rules.validate(List(Condition.Kind(TxKind.Income)), Nil).isLeft)
    assert(Rules.validate(List(Condition.Amount(AmountOp.Between, Some(10), Some(5))), List(Action.AddTags(Nil))).isLeft)
    assert(Rules.validate(List(Condition.Text(TextField.Title, TextOp.ContainsAny, List(" "))), List(Action.AddTags(Nil))).isLeft)
    assert(Rules.validate(starbucks.conditions, starbucks.actions).isRight)
  }

  test("conditions and actions round-trip through JSON") {
    import RuleJson.given
    val conds = starbucks.conditions ++ List(Condition.Payee(in = true, List(PayeeId.assume(UUID.randomUUID()))), Condition.Kind(TxKind.Expense),
                                            Condition.Wallet(in = false, List(w.id)), Condition.DayOfMonth(1, 15))
    assertEquals(decode[List[Condition]](conds.asJson.noSpaces), Right(conds))
    assertEquals(decode[List[Action]](starbucks.actions.asJson.noSpaces), Right(starbucks.actions))
    assert(decode[List[Condition]]("""[{"field":"colour"}]""").isLeft)
  }

  test("payees match on normalised names and aliases, preferring exact then longest") {
    val amazon = Payee(PayeeId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Amazon"), List("AMZN Mktp"), None)
    val prime  = Payee(PayeeId.assume(UUID.randomUUID()), owner, Title.applyUnsafe("Amazon Prime"), Nil, None)
    val ps = List(amazon, prime)
    assertEquals(Payees.find(ps, "AMZN Mktp US*2K4").map(_.id), Some(amazon.id))
    assertEquals(Payees.find(ps, "amazon prime video").map(_.id), Some(prime.id))
    assertEquals(Payees.find(ps, "AMAZON").map(_.id), Some(amazon.id))
    assertEquals(Payees.find(ps, "Amazonia cafe"), None)                 // whole words only
    assertEquals(Payees.normalize("  Uber *Trip 1234 "), "uber trip")
  }
}
