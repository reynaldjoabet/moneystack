package wealth.accounting.service

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.service.LedgerService.SystemAccounts
import wealth.accounting.service.TradeService.NewLine

import java.time.Instant
import java.util.UUID

class ResolveLinesSuite extends munit.FunSuite {

  private def item(sku: String, kind: ItemKind, sell: Option[Long], cost: Option[Long], active: Boolean = true) =
    Item(ItemId.assume(UUID.randomUUID()), Sku.applyUnsafe(sku), Title.applyUnsafe(sku), kind,
         sell.map(PriceMinor.assume(_)), cost.map(PriceMinor.assume(_)),
         SystemAccounts.Sales.id, if (kind == ItemKind.Inventory) SystemAccounts.CostOfGoodsSold.id else SystemAccounts.GeneralExpenses.id,
         Option.when(kind == ItemKind.Inventory)(SystemAccounts.Inventory.id), active, Instant.EPOCH)

  private val widget  = item("WIDGET", ItemKind.Inventory, Some(2_500), Some(1_000))
  private val consult = item("CONSULT", ItemKind.Service, Some(15_000), None)
  private val retired = item("OLD", ItemKind.Service, Some(1), None, active = false)
  private val items   = List(widget, consult, retired).map(i => i.id -> i).toMap

  private val vat   = TaxRate(TaxRateId.assume(UUID.randomUUID()), TaxCode.applyUnsafe("VAT20"), Title.applyUnsafe("VAT"),
                              Bps.assume(2_000), active = true, Instant.EPOCH)
  private val rates = Map(vat.id -> vat)

  private def qty(q: String) = Quantity.assume(BigDecimal(q))
  private def newLine(itemId: Option[ItemId], q: String, price: Option[Long] = None, account: Option[AccountId] = None,
                      tax: Option[TaxRateId] = None) =
    NewLine(itemId, None, qty(q), price.map(PriceMinor.assume(_)), account, tax)

  test("item lines take prices and accounts from the item") {
    val Right(List(sale)) = TradeService.resolveLines(DocumentKind.Invoice, List(newLine(Some(widget.id), "2", tax = Some(vat.id))), items, rates): @unchecked
    assertEquals(sale.accountId, SystemAccounts.Sales.id)
    assertEquals(sale.unitPriceMinor.value: Long, 2_500L)
    assertEquals(sale.amountMinor.value: Long, 5_000L)
    assertEquals(sale.taxMinor.value: Long, 1_000L)
    assertEquals(sale.taxBps.map(b => b.value: Int), Some(2_000))

    val Right(List(purchase)) = TradeService.resolveLines(DocumentKind.Bill, List(newLine(Some(widget.id), "4")), items, rates): @unchecked
    assertEquals(purchase.accountId, SystemAccounts.Inventory.id)
    assertEquals(purchase.amountMinor.value: Long, 4_000L)
  }

  test("inventory lines ignore an account override; other item lines honour it") {
    val other = AccountId.assume(UUID.randomUUID())
    val Right(List(inv, svc)) = TradeService.resolveLines(DocumentKind.Bill,
      List(newLine(Some(widget.id), "1", account = Some(other)), newLine(Some(consult.id), "1", Some(100), account = Some(other))),
      items, rates): @unchecked
    assertEquals(inv.accountId, SystemAccounts.Inventory.id)
    assertEquals(svc.accountId, other)
  }

  test("free lines need an account and a price; unknown and inactive references fail") {
    val acct = SystemAccounts.GeneralExpenses.id
    assertEquals(TradeService.resolveLines(DocumentKind.Bill, List(newLine(None, "1", Some(100))), items, rates),
                 Left(LedgerError.LineAccountRequired(1)))
    assertEquals(TradeService.resolveLines(DocumentKind.Bill, List(newLine(None, "1", account = Some(acct))), items, rates),
                 Left(LedgerError.LinePriceRequired(1)))
    assertEquals(TradeService.resolveLines(DocumentKind.Bill, List(newLine(Some(consult.id), "1")), items, rates),
                 Left(LedgerError.LinePriceRequired(1)))   // no cost price on the service
    assertEquals(TradeService.resolveLines(DocumentKind.Invoice, List(newLine(Some(retired.id), "1")), items, rates),
                 Left(LedgerError.InactiveItem(retired.sku)))
    val ghost = TaxRateId.assume(UUID.randomUUID())
    assertEquals(TradeService.resolveLines(DocumentKind.Invoice, List(newLine(Some(consult.id), "1", tax = Some(ghost))), items, rates),
                 Left(LedgerError.UnknownTaxRate(ghost)))
  }

  test("fractional quantities round the line amount half-even") {
    val Right(List(l)) = TradeService.resolveLines(DocumentKind.Invoice,
      List(newLine(None, "0.333333", Some(1_000), account = Some(SystemAccounts.Sales.id))), items, rates): @unchecked
    assertEquals(l.amountMinor.value: Long, 333L)
  }
}
