package moneystack.accounting.http

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.service.{CatalogService, TradeService}

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

/**
 * Contacts, tax rates, items, invoices/bills, receipts/bill payments, stock
 * adjustments, and the sub-ledger reports.
 */
final class TradeRoutes[F[_]: Async](
    auth: Request[F] => F[Option[UserId]],
    catalog: CatalogService[F],
    trade: TradeService[F]
) extends RouteSupport[F](auth) {

  import AccountingJson.given

  /** `/invoices` and `/bills`. */
  private object Docs {
    def unapply(s: String): Option[DocumentKind] = s match {
      case "invoices" => Some(DocumentKind.Invoice)
      case "bills"    => Some(DocumentKind.Bill)
      case _          => None
    }
  }

  /** `/receipts` settle invoices, `/bill-payments` settle bills. */
  private object Settles {
    def unapply(s: String): Option[DocumentKind] = s match {
      case "receipts"      => Some(DocumentKind.Invoice)
      case "bill-payments" => Some(DocumentKind.Bill)
      case _               => None
    }
  }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- contacts ----

    case req @ GET -> Root / "accountant" / "contacts" :? KindParam(kind) =>
      accountant(req)(_ => optParam(kind).fold(bad, k => catalog.contacts(k).flatMap(Ok(_))))

    case req @ POST -> Root / "accountant" / "contacts" =>
      accountant(req)(_ => decoded[CatalogService.NewContact](req)(b => catalog.createContact(b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "accountant" / "contacts" / ContactIdVar(id) / "active" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ActiveBody](req)(b => catalog.setContactActive(id, b.active).flatMap(_.fold(NotFound())(Ok(_))))
      )

    // ---- tax rates ----

    case req @ GET -> Root / "accountant" / "tax-rates" =>
      accountant(req)(_ => catalog.taxRates.flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / "tax-rates" =>
      accountant(req)(_ => decoded[CatalogService.NewTaxRate](req)(b => catalog.createTaxRate(b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "accountant" / "tax-rates" / TaxRateIdVar(id) / "active" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ActiveBody](req)(b => catalog.setTaxRateActive(id, b.active).flatMap(_.fold(NotFound())(Ok(_))))
      )

    // ---- items & inventory ----

    case req @ GET -> Root / "accountant" / "items" =>
      accountant(req)(_ => catalog.items.flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / "items" =>
      accountant(req)(_ => decoded[CatalogService.NewItem](req)(b => catalog.createItem(b).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "accountant" / "items" / ItemIdVar(id) / "active" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ActiveBody](req)(b => catalog.setItemActive(id, b.active).flatMap(_.fold(NotFound())(Ok(_))))
      )

    case req @ GET -> Root / "accountant" / "inventory" / "adjustments" :? Limit(l) =>
      accountant(req)(_ => trade.adjustments(limit(l)).flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / "inventory" / "adjustments" =>
      accountant(req)(u => decoded[TradeService.NewAdjustment](req)(b => trade.adjustStock(b, u).flatMap(respond(_)(Created(_)))))

    // ---- invoices & bills ----

    case req @ GET -> Root / "accountant" / Docs(kind) :? From(from) +& To(to) +& ContactParam(contact) +& Limit(l) =>
      accountant(req)(_ =>
        optParam(contact).fold(bad, c => period(from, to)((f, t) => trade.documents(kind, f, t, c, limit(l)).flatMap(Ok(_))))
      )

    case req @ POST -> Root / "accountant" / Docs(kind) =>
      accountant(req)(u =>
        decoded[TradeService.NewDocument](req)(b => trade.createDocument(kind, b, u).flatMap(respond(_)(Created(_))))
      )

    case req @ GET -> Root / "accountant" / Docs(kind) / DocumentIdVar(id) =>
      accountant(req)(_ => trade.document(id).map(_.filter(_.document.kind == kind)).flatMap(_.fold(NotFound())(Ok(_))))

    case req @ POST -> Root / "accountant" / Docs(_) / DocumentIdVar(id) / "void" =>
      accountant(req)(u =>
        decoded[AccountingJson.DateBody](req)(b => trade.voidDocument(id, b.date, u).flatMap(respond(_)(Ok(_))))
      )

    // ---- receipts & bill payments ----

    case req @ GET -> Root / "accountant" / Settles(kind) :? From(from) +& To(to) +& Limit(l) =>
      accountant(req)(_ => period(from, to)((f, t) => trade.settlements(kind, f, t, limit(l)).flatMap(Ok(_))))

    case req @ POST -> Root / "accountant" / Settles(kind) =>
      accountant(req)(u =>
        decoded[TradeService.NewSettlement](req)(b => trade.settle(kind, b, u).flatMap(respond(_)(Created(_))))
      )

    case req @ GET -> Root / "accountant" / Settles(kind) / SettlementIdVar(id) =>
      accountant(req)(_ => trade.settlement(id).map(_.filter(_.settlement.kind == kind)).flatMap(_.fold(NotFound())(Ok(_))))

    case req @ POST -> Root / "accountant" / Settles(_) / SettlementIdVar(id) / "void" =>
      accountant(req)(u =>
        decoded[AccountingJson.DateBody](req)(b => trade.voidSettlement(id, b.date, u).flatMap(respond(_)(Ok(_))))
      )

    // ---- reports ----

    case req @ GET -> Root / "accountant" / "reports" / "ar-aging" :? AsOf(d) =>
      accountant(req)(_ => asOf(d)(trade.aging(DocumentKind.Invoice, _).flatMap(Ok(_))))

    case req @ GET -> Root / "accountant" / "reports" / "ap-aging" :? AsOf(d) =>
      accountant(req)(_ => asOf(d)(trade.aging(DocumentKind.Bill, _).flatMap(Ok(_))))

    case req @ GET -> Root / "accountant" / "reports" / "sales-tax" :? From(from) +& To(to) =>
      accountant(req)(_ => period(from, to)((f, t) => trade.salesTax(f, t).flatMap(respond(_)(Ok(_)))))

    case req @ GET -> Root / "accountant" / "reports" / "inventory-valuation" :? AsOf(d) =>
      accountant(req)(_ => asOf(d)(trade.inventoryValuation(_).flatMap(Ok(_))))
  }
}
