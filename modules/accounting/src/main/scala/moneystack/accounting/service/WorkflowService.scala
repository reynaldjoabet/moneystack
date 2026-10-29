package moneystack.accounting.service

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.db.{Contacts, Documents, Office}
import moneystack.platform.Db

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.deriveCodec
import io.circe.parser.decode
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import java.util.UUID

/**
 * The document lifecycle around [[TradeService]], after Akaunting: drafts
 * that post nothing until issued, sent/reminder markers and a history of
 * each document, and recurring templates issued on schedule.
 */
trait WorkflowService[F[_]] {
  def saveDraft(kind: DocumentKind, id: Option[DraftId], b: TradeService.NewDocument, by: UserId): F[Either[LedgerError, WorkflowService.DraftView]]
  def drafts(kind: DocumentKind): F[List[WorkflowService.DraftView]]
  def deleteDraft(id: DraftId): F[Either[LedgerError, Unit]]
  /** Posts the draft as a numbered document and removes it. */
  def issueDraft(id: DraftId, by: UserId): F[Either[LedgerError, DocumentDetails]]

  def markSent(document: DocumentId, note: Option[Body], by: UserId): F[Either[LedgerError, DocumentEvent]]
  def remind(document: DocumentId, note: Option[Body], by: UserId): F[Either[LedgerError, DocumentEvent]]
  def history(document: DocumentId): F[Either[LedgerError, List[DocumentEvent]]]

  def createRecurring(kind: DocumentKind, b: WorkflowService.NewRecurring, by: UserId): F[Either[LedgerError, WorkflowService.RecurringView]]
  def recurring: F[List[WorkflowService.RecurringView]]
  def setRecurringActive(id: RecurringId, active: Boolean): F[Option[WorkflowService.RecurringView]]
  /** Issues every template occurrence due on or before `asOf`; safe to run repeatedly. */
  def runRecurring(asOf: LocalDate, by: UserId): F[WorkflowService.RunReport]
}

object WorkflowService {

  final case class DraftView(draft: Draft, document: TradeService.NewDocument)

  /** `document.date` and `dueDate` give the first issue and its payment terms; later issues keep the same gap. */
  final case class NewRecurring(every: Every, interval: Option[Int], endDate: Option[LocalDate], maxOccurrences: Option[Int],
                                document: TradeService.NewDocument)

  final case class RecurringView(recurring: Recurring, document: TradeService.NewDocument, nextDue: Option[LocalDate])

  final case class RunReport(issued: List[DocumentId], failed: List[(RecurringId, String)])

  /** JSON for stored document requests. Quantities and rates must be positive, as on the wire. */
  object Payload {
    private given Decoder[Quantity]     = Decoder.decodeBigDecimal.emap(Quantity.positive)
    private given Encoder[Quantity]     = Encoder.encodeBigDecimal.contramap(_.value)
    private given Decoder[ExchangeRate] = Decoder.decodeBigDecimal.emap(ExchangeRate.positive)
    private given Encoder[ExchangeRate] = Encoder.encodeBigDecimal.contramap(_.value)
    given Codec[TradeService.NewLine]     = deriveCodec
    given Codec[TradeService.NewDocument] = deriveCodec

    def write(d: TradeService.NewDocument): String = d.asJson.noSpaces
    def read(s: String): Either[LedgerError, TradeService.NewDocument] =
      decode[TradeService.NewDocument](s).leftMap(e => LedgerError.InvalidDraft(e.getMessage))
  }

  def make[F[_]: Async](db: Db[F], trade: TradeService[F], contacts: Contacts[F], documents: Documents[F], office: Office[F]): WorkflowService[F] =
    new WorkflowService[F] {

      private type R[A] = EitherT[F, LedgerError, A]
      private def lift[A](fa: F[A]): R[A] = EitherT.liftF(fa)

      private def checkContact(kind: DocumentKind, id: ContactId): R[Unit] =
        EitherT(contacts.find(id).map {
          case None                                    => Left(LedgerError.UnknownContact(id))
          case Some(c) if c.kind != kind.contactKind   => Left(LedgerError.ContactKindMismatch(id, kind.contactKind.key))
          case Some(_)                                 => Right(())
        })

      private def view(d: Draft): Either[LedgerError, DraftView] = Payload.read(d.payload).map(DraftView(d, _))

      def saveDraft(kind: DocumentKind, id: Option[DraftId], b: TradeService.NewDocument, by: UserId): F[Either[LedgerError, DraftView]] =
        (for {
          _        <- checkContact(kind, b.contactId)
          existing <- id.traverse(i => EitherT.fromOptionF(office.findDraft(i), LedgerError.DraftNotFound(i): LedgerError))
          _        <- EitherT.cond[F](existing.forall(_.kind == kind), (), LedgerError.InvalidDraft("a draft can't change between invoice and bill"): LedgerError)
          newId    <- lift(Async[F].delay(DraftId.assume(UUID.randomUUID())))
          now      <- lift(Async[F].delay(Instant.now()))
          saved    <- lift(office.saveDraft(Draft(existing.fold(newId)(_.id), kind, b.contactId, Payload.write(b), existing.fold(by)(_.createdBy),
                                                  existing.fold(now)(_.createdAt), now)))
        } yield DraftView(saved, b)).value

      def drafts(kind: DocumentKind): F[List[DraftView]] =
        office.drafts(kind).map(_.flatMap(d => view(d).toOption))

      def deleteDraft(id: DraftId): F[Either[LedgerError, Unit]] =
        db.tx(office.deleteDraftIn(_, id)).map(found => Either.cond(found, (), LedgerError.DraftNotFound(id)))

      def issueDraft(id: DraftId, by: UserId): F[Either[LedgerError, DocumentDetails]] =
        (for {
          d      <- EitherT.fromOptionF(office.findDraft(id), LedgerError.DraftNotFound(id): LedgerError)
          body   <- EitherT.fromEither[F](Payload.read(d.payload))
          issued <- EitherT(trade.createDocument(d.kind, body, by))
          _      <- lift(db.tx(office.deleteDraftIn(_, id)))
        } yield issued).value

      private def event(document: DocumentId, kind: DocumentEventKind, note: Option[Body], by: UserId): F[Either[LedgerError, DocumentEvent]] =
        (for {
          doc <- EitherT.fromOptionF(documents.find(document), LedgerError.DocumentNotFound(document): LedgerError)
          _   <- EitherT.cond[F](doc.voidedOn.isEmpty, (), LedgerError.AlreadyVoided(doc.voidedOn.getOrElse(doc.date)): LedgerError)
          now <- lift(Async[F].delay(Instant.now()))
          e    = DocumentEvent(document, kind, note, Some(by), now)
          _   <- lift(office.addEvent(e))
        } yield e).value

      def markSent(document: DocumentId, note: Option[Body], by: UserId): F[Either[LedgerError, DocumentEvent]] =
        event(document, DocumentEventKind.Sent, note, by)

      def remind(document: DocumentId, note: Option[Body], by: UserId): F[Either[LedgerError, DocumentEvent]] =
        event(document, DocumentEventKind.Reminder, note, by)

      def history(document: DocumentId): F[Either[LedgerError, List[DocumentEvent]]] =
        documents.find(document).flatMap {
          case None    => Async[F].pure(Left(LedgerError.DocumentNotFound(document)))
          case Some(_) => office.events(document).map(Right(_))
        }

      private def recurringView(r: Recurring): Option[RecurringView] =
        Payload.read(r.payload).toOption.map(RecurringView(r, _, Option.when(r.active)(r.due(r.nextIndex)).flatten))

      def createRecurring(kind: DocumentKind, b: NewRecurring, by: UserId): F[Either[LedgerError, RecurringView]] =
        (for {
          _   <- checkContact(kind, b.document.contactId)
          _   <- EitherT.cond[F](b.interval.forall(i => i >= 1 && i <= 366), (), LedgerError.InvalidDraft("interval must be 1-366"): LedgerError)
          _   <- EitherT.cond[F](b.maxOccurrences.forall(_ > 0), (), LedgerError.InvalidDraft("max occurrences must be positive"): LedgerError)
          _   <- EitherT.cond[F](b.endDate.forall(!_.isBefore(b.document.date)), (), LedgerError.InvalidDraft("the end date is before the first issue"): LedgerError)
          _   <- EitherT.cond[F](b.document.lines.nonEmpty, (), LedgerError.NoLines: LedgerError)
          id  <- lift(Async[F].delay(RecurringId.assume(UUID.randomUUID())))
          now <- lift(Async[F].delay(Instant.now()))
          r   <- lift(office.createRecurring(Recurring(id, kind, b.document.contactId, Payload.write(b.document), b.every, b.interval.getOrElse(1),
                                                       b.document.date, b.endDate, b.maxOccurrences, 0, active = true, by, now)))
        } yield RecurringView(r, b.document, r.due(0))).value

      def recurring: F[List[RecurringView]] = office.recurring().map(_.flatMap(recurringView))

      def setRecurringActive(id: RecurringId, active: Boolean): F[Option[RecurringView]] =
        office.setRecurringActive(id, active).map(_.flatMap(recurringView))

      def runRecurring(asOf: LocalDate, by: UserId): F[RunReport] =
        office.recurring().flatMap { all =>
          all.filter(_.active).foldLeftM(RunReport(Nil, Nil)) { (report, r) =>
            Payload.read(r.payload) match {
              case Left(err) => Async[F].pure(report.copy(failed = report.failed :+ (r.id -> err.toString)))
              case Right(template) =>
                val terms = template.dueDate.map(d => ChronoUnit.DAYS.between(template.date, d))
                // Issue each due occurrence in turn; stop at the first failure so nothing is skipped.
                def loop(n: Int, acc: RunReport): F[RunReport] =
                  r.due(n).filter(!_.isAfter(asOf)) match {
                    case None => Async[F].pure(acc)
                    case Some(date) =>
                      val body = template.copy(date = date, dueDate = terms.map(date.plusDays))
                      trade.createDocument(r.kind, body, by).flatMap {
                        case Left(err) => Async[F].pure(acc.copy(failed = acc.failed :+ (r.id -> err.toString)))
                        case Right(doc) =>
                          db.tx(office.claimRunIn(_, r.id, n, doc.document.id)).flatMap { claimed =>
                            if (claimed) loop(n + 1, acc.copy(issued = acc.issued :+ doc.document.id))
                            // Another run issued this occurrence first: withdraw the duplicate.
                            else trade.voidDocument(doc.document.id, date, by).as(acc)
                          }
                      }
                  }
                loop(r.nextIndex, report)
            }
          }
        }
    }
}
