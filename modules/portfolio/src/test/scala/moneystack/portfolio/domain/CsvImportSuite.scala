package moneystack.portfolio.domain

import moneystack.domain.*

import java.util.UUID

class CsvImportSuite extends munit.FunSuite {

  test("parses quoted cells, optional columns and any column order") {
    val csv =
      """Type,Date,Symbol,Quantity,Price,Fee,Currency,Note
        |buy,2026-01-02,AAPL,10,150.5,1,USD,"first, lot"
        |deposit,2026-01-01,,,,0,usd,
        |""".stripMargin
    val p = CsvImport.parse(csv)
    assertEquals(p.errors, Nil)
    assertEquals(p.rows.map(_.kind), List(ActivityType.Buy, ActivityType.Deposit))
    assertEquals(p.rows.head.unitPrice, Some(BigDecimal("150.5")))
    assertEquals(p.rows.head.note.map(n => n.value: String), Some("first, lot"))
    assertEquals(p.rows(1).currency, CurrencyCode.USD)
  }

  test("reports every bad line and missing columns") {
    assertEquals(CsvImport.parse("date,type\n2026-01-01,buy").errors, List("missing columns: currency"))
    val p = CsvImport.parse("date,type,currency,quantity\n2026-13-01,buy,USD,1\n2026-01-01,teleport,USD,1\n2026-01-01,buy,USD,ten")
    assertEquals(p.errors.size, 3)
    assert(p.errors.head.startsWith("line 2"))
  }

  test("import keys are stable per account and ignore number formatting") {
    val a = CsvImport.parse("date,type,currency,amount\n2026-01-01,deposit,USD,100.00").rows.head
    val b = CsvImport.parse("date,type,currency,amount\n2026-01-01,deposit,USD,100").rows.head
    val acct = InvestmentAccountId.assume(UUID.randomUUID())
    assertEquals(a.key(acct), b.key(acct))
    assertNotEquals(a.key(acct), a.key(InvestmentAccountId.assume(UUID.randomUUID())))
  }

  test("splits CSV with doubled quotes") {
    assertEquals(CsvImport.split("a,\"b \"\"q\"\" c\",d"), List("a", "b \"q\" c", "d"))
  }
}
