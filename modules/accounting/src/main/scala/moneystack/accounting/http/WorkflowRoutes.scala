package moneystack.accounting.http

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.service.{CashService, TradeService, WorkflowService}

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

/** Drafts, document history, recurring documents, direct income/expense, transfers and reconciliation. */
final class WorkflowRoutes[F[_]: Async](auth: Request[F] => F[Option[UserId]], workflow: WorkflowService[F], cash: CashService[F])
    extends RouteSupport[F](auth) {

  import AccountingJson.given

  private val DraftIdVar          = new UuidVar(DraftId.assume)
  private val RecurringIdVar      = new UuidVar(RecurringId.assume)
  private val ReconciliationIdVar = new UuidVar(ReconciliationId.assume)

  private object Docs {
    def unapply(s: String): Option[DocumentKind] = s match {
      case "invoices" => Some(DocumentKind.Invoice)
      case "bills"    => Some(DocumentKind.Bill)
      case _          => None
    }
  }

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    // ---- drafts ----

    case req @ GET -> Root / "accountant" / Docs(kind) / "drafts" =>
      accountant(req)(_ => workflow.drafts(kind).flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / Docs(kind) / "drafts" =>
      accountant(req)(u =>
        decoded[TradeService.NewDocument](req)(b => workflow.saveDraft(kind, None, b, u).flatMap(respond(_)(Created(_))))
      )

    case req @ PUT -> Root / "accountant" / Docs(kind) / "drafts" / DraftIdVar(id) =>
      accountant(req)(u => decoded[TradeService.NewDocument](req)(b => workflow.saveDraft(kind, Some(id), b, u).flatMap(respond(_)(Ok(_)))))

    case req @ DELETE -> Root / "accountant" / "drafts" / DraftIdVar(id) =>
      accountant(req)(_ => workflow.deleteDraft(id).flatMap(respond(_)(_ => NoContent())))

    case req @ POST -> Root / "accountant" / "drafts" / DraftIdVar(id) / "issue" =>
      accountant(req)(u => workflow.issueDraft(id, u).flatMap(respond(_)(Created(_))))

    // ---- history ----

    case req @ POST -> Root / "accountant" / Docs(_) / DocumentIdVar(id) / "sent" =>
      accountant(req)(u => decoded[AccountingJson.NoteBody](req)(b => workflow.markSent(id, b.note, u).flatMap(respond(_)(Ok(_)))))

    case req @ POST -> Root / "accountant" / Docs(_) / DocumentIdVar(id) / "remind" =>
      accountant(req)(u => decoded[AccountingJson.NoteBody](req)(b => workflow.remind(id, b.note, u).flatMap(respond(_)(Ok(_)))))

    case req @ GET -> Root / "accountant" / Docs(_) / DocumentIdVar(id) / "history" =>
      accountant(req)(_ => workflow.history(id).flatMap(respond(_)(Ok(_))))

    // ---- recurring ----

    case req @ GET -> Root / "accountant" / "recurring" =>
      accountant(req)(_ => workflow.recurring.flatMap(Ok(_)))

    case req @ POST -> Root / "accountant" / Docs(kind) / "recurring" =>
      accountant(req)(u =>
        decoded[WorkflowService.NewRecurring](req)(b => workflow.createRecurring(kind, b, u).flatMap(respond(_)(Created(_))))
      )

    case req @ POST -> Root / "accountant" / "recurring" / RecurringIdVar(id) / "active" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ActiveBody](req)(b => workflow.setRecurringActive(id, b.active).flatMap(_.fold(NotFound())(Ok(_))))
      )

    case req @ POST -> Root / "accountant" / "recurring" / "run" :? AsOf(d) =>
      accountant(req)(u => asOf(d)(day => workflow.runRecurring(day, u).flatMap(Ok(_))))

    // ---- cash ----

    case req @ POST -> Root / "accountant" / "banking" / "money-in" =>
      accountant(req)(u => decoded[CashService.NewMovement](req)(b => cash.moneyIn(b, u).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "accountant" / "banking" / "money-out" =>
      accountant(req)(u => decoded[CashService.NewMovement](req)(b => cash.moneyOut(b, u).flatMap(respond(_)(Created(_)))))

    case req @ POST -> Root / "accountant" / "banking" / "transfers" =>
      accountant(req)(u => decoded[CashService.NewTransfer](req)(b => cash.transfer(b, u).flatMap(respond(_)(Created(_)))))

    // ---- reconciliation ----

    case req @ GET -> Root / "accountant" / "reconciliations" :? AccountParam(acct) =>
      accountant(req)(_ =>
        optParam(acct).fold(
          bad,
          {
            case Some(a) => cash.reconciliations(a).flatMap(Ok(_))
            case None    => bad("account is required")
          }
        )
      )

    case req @ POST -> Root / "accountant" / "reconciliations" =>
      accountant(req)(u => decoded[CashService.NewReconciliation](req)(b => cash.startReconciliation(b, u).flatMap(respond(_)(Created(_)))))

    case req @ GET -> Root / "accountant" / "reconciliations" / ReconciliationIdVar(id) =>
      accountant(req)(_ => cash.reconciliation(id).flatMap(respond(_)(Ok(_))))

    case req @ POST -> Root / "accountant" / "reconciliations" / ReconciliationIdVar(id) / "lines" =>
      accountant(req)(_ =>
        decoded[AccountingJson.ClearBody](req)(b =>
          cash.setCleared(id, b.lines.map(l => l.entryId -> l.lineNo), b.cleared).flatMap(respond(_)(Ok(_)))
        )
      )

    case req @ POST -> Root / "accountant" / "reconciliations" / ReconciliationIdVar(id) / "close" =>
      accountant(req)(_ => cash.closeReconciliation(id).flatMap(respond(_)(Ok(_))))
  }
}
