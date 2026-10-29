package wealth.accounting.service

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.db.{ExchangeRates, Journals, LedgerAccounts}

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import skunk.Session

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Journal posting shared by the services that write entries as part of a
 * larger transaction (documents, settlements, inventory, bank feed).
 */
final class Posting[F[_]: Async](
    val baseCurrency: CurrencyCode,
    ledgerAccounts: LedgerAccounts[F],
    journals: Journals[F],
    exchangeRates: ExchangeRates[F]
) {

  def chart: F[Map[AccountId, Account]] =
    ledgerAccounts.list().map(_.map(a => a.id -> a).toMap)

  /** Base currency is always 1; otherwise an explicit rate wins over the stored daily rate. */
  def rate(currency: CurrencyCode, explicit: Option[ExchangeRate], date: LocalDate): F[Either[LedgerError, ExchangeRate]] =
    if (currency == baseCurrency) Async[F].pure(Right(ExchangeRate.One))
    else explicit match {
      case Some(r) => Async[F].pure(Right(r))
      case None    => exchangeRates.rateOn(currency, date).map(_.toRight(LedgerError.MissingExchangeRate(currency, date)))
    }

  def header(
      date: LocalDate,
      reference: Option[JournalRef],
      memo: Option[Body],
      currency: CurrencyCode,
      rate: ExchangeRate,
      source: JournalSource,
      sourceId: Option[UUID],
      by: Option[UserId]
  ): F[JournalHeader] =
    for {
      id  <- Async[F].delay(JournalEntryId.assume(UUID.randomUUID()))
      now <- Async[F].delay(Instant.now())
    } yield JournalHeader(id, date, reference, memo, currency, rate, source, sourceId, by, now)

  /** Validates and posts inside `s`; anything but a fresh posting is an error, rolling back the caller. */
  def postIn(
      s: Session[F],
      header: JournalHeader,
      lines: List[JournalLine],
      chart: Map[AccountId, Account],
      allowInactive: Boolean = false
  ): EitherT[F, LedgerError, JournalEntry] =
    for {
      entry <- EitherT.fromEither[F](Ledger.entry(header, lines, baseCurrency, chart, allowInactive))
      saved <- EitherT(journals.postIn(s, entry).map {
                 case Journals.Outcome.Posted(e) => Right(e)
                 case Journals.Outcome.Locked(l) => Left(LedgerError.PeriodLocked(header.date, l.lockThrough))
                 case Journals.Outcome.Duplicate(existing) =>
                   val sid = header.sourceId.getOrElse(header.id.value)
                   if (header.source == JournalSource.Reversal) Left(LedgerError.AlreadyReversed(JournalEntryId.assume(sid), existing))
                   else Left(LedgerError.DuplicatePosting(header.source, sid))
               })
    } yield saved

  /** Posts the entry that cancels `entryId`, dated `date`, inside `s`. */
  def reverseIn(
      s: Session[F],
      entryId: JournalEntryId,
      date: LocalDate,
      by: Option[UserId],
      chart: Map[AccountId, Account]
  ): EitherT[F, LedgerError, JournalEntry] =
    for {
      orig <- EitherT.fromOptionF(journals.findIn(s, entryId), LedgerError.EntryNotFound(entryId): LedgerError)
      h    <- EitherT.liftF(header(date, orig.header.reference, Some(Body.assume(s"Reversal of entry ${entryId.value}")),
                                   orig.header.currency, orig.header.exchangeRate, JournalSource.Reversal, Some(entryId.value), by))
      e    <- postIn(s, h, Ledger.reversalOf(orig), chart, allowInactive = true)
    } yield e
}
