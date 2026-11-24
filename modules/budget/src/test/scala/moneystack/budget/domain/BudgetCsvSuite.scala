package moneystack.budget.domain

import Fixtures.*

import java.util.UUID

class BudgetCsvSuite extends munit.FunSuite {

  test("signed amounts pick the kind; explicit types win; descriptions stand in for titles") {
    val p = BudgetCsv.parse(
      """Date,Description,Amount,Category
        |2026-03-01,Salary,"2,500.00",Salary
        |2026-03-02,Coffee,-4.5,Dining
        |2026-03-03,Refund,12.00,
        |""".stripMargin
    )
    assertEquals(p.errors, Nil)
    assertEquals(p.rows.map(r => (r.kind, r.amountMinor)), List(TxKind.Income -> 250_000L, TxKind.Expense -> 450L, TxKind.Income -> 1_200L))
    assertEquals(p.rows.map(_.category), List(Some("Salary"), Some("Dining"), None))
    val typed = BudgetCsv.parse("date,title,amount,type\n2026-03-01,Gift,10,expense")
    assertEquals(typed.rows.head.kind, TxKind.Expense)
  }

  test("errors name the line; transfers and zero amounts are refused") {
    val p = BudgetCsv.parse("date,title,amount,type\n2026-02-30,a,1,\n2026-03-01,b,0,\n2026-03-01,c,5,transfer\n2026-03-01,,5,")
    assertEquals(p.errors.size, 4)
    assert(p.errors.forall(_.startsWith("line ")))
    assertEquals(BudgetCsv.parse("date,amount\n").errors, List("missing columns: description"))
  }

  test("keys ignore title case and spacing; export quotes awkward cells") {
    val w = WalletId.assume(UUID.randomUUID())
    val a = BudgetCsv.parse("date,title,amount\n2026-03-01,Coffee Shop,-4.50").rows.head
    val b = BudgetCsv.parse("date,title,amount\n2026-03-01, coffee shop ,-4.5").rows.head
    assertEquals(a.key(w), b.key(w))

    val wal = wallet()
    val t   = tx(wal, TxKind.Expense, 1234, d(3, 1), title = "Lunch, with \"Ann\"")
    val out = BudgetCsv.write(List(t), Map(wal.id -> wal), Map.empty, Map.empty, Map.empty)
    assert(out.linesIterator.next().startsWith("date,type,wallet,amount"))
    assert(out.contains("12.34"))
    assert(out.contains("\"Lunch, with \"\"Ann\"\"\""))
  }

  test("amounts as banks export them") {
    val cases = List(
      "1234.56"     -> Some("1234.56"),
      "-12"         -> Some("-12"),
      "+12"         -> Some("12"),
      "(1,234.56)"  -> Some("-1234.56"),
      "1234.56 DR"  -> Some("-1234.56"),
      "1234.56 CR"  -> Some("1234.56"),
      "50.00dr."    -> Some("-50.00"),
      "-5.00 DR"    -> None,         // a sign and DR contradict each other
      "€1.234,56"   -> Some("1234.56"),
      "$-5.00"      -> Some("-5.00"),
      "\u221242.10" -> Some("-42.10"),
      "1 234,5"     -> Some("1234.5"),
      "3,250,000"   -> Some("3250000"),
      "1.500.000"   -> Some("1500000"),
      "1,234"       -> Some("1234"), // as this importer always read it
      "12,5"        -> Some("12.5"),
      "1,23,456"    -> None,
      "12abc"       -> None,
      ""            -> None
    )
    cases.foreach { case (in, out) => assertEquals(BudgetCsv.amount(in), out.map(BigDecimal(_)), in) }
  }
}
