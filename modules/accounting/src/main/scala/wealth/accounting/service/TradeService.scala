package wealth.accounting.service

import wealth.domain.*
import wealth.accounting.domain.*
import wealth.accounting.db.{Contacts, Documents, Items, Journals, Settlements, TaxRates}
import wealth.platform.Db
import wealth.accounting.service.LedgerService.SystemAccounts

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import skunk.Session

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Sales and purchases: invoices and bills (with inventory receipt/issue and
 * cost of goods sold), customer receipts and vendor payments (with realised
 * exchange differences), stock adjustments, and the sub-ledger reports.
 * Each operation posts its journal entries and sub-ledger rows atomically.
 */
trait TradeService[F[_]] {
  def createDocument(kind: DocumentKind, b: TradeService.NewDocument, by: UserId): F[Either[LedgerError, DocumentDetails]]
  def document(id: DocumentId): F[Option[DocumentDetails]]
  def documents(kind: DocumentKind, from: LocalDate, to: LocalDate, contact: Option[ContactId], limit: Int): F[List[TradeService.DocumentSummary]]
  /** Reverses an unpaid document's postings and stock movements. */
  def voidDocument(id: DocumentId, date: LocalDate, by: UserId): F[Either[LedgerError, DocumentDetails]]

  def settle(kind: DocumentKind, b: TradeService.NewSettlement, by: UserId): F[Either[LedgerError, SettlementDetails]]
  def settlement(id: SettlementId): F[Option[SettlementDetails]]
  def settlements(kind: DocumentKind, from: LocalDate, to: LocalDate, limit: Int): F[List[Settlement]]
  def voidSettlement(id: SettlementId, date: LocalDate, by: UserId): F[Either[LedgerError, SettlementDetails]]

  def adjustStock(b: TradeService.NewAdjustment, by: UserId): F[Either[LedgerError, InventoryAdjustment]]
  def adjustments(limit: Int): F[List[InventoryAdjustment]]

  def aging(kind: DocumentKind, asOf: LocalDate): F[TradeReports.Aging]
  def salesTax(from: LocalDate, to: LocalDate): F[Either[LedgerError, TradeReports.SalesTax]]
  def inventoryValuation(asOf: LocalDate): F[TradeReports.InventoryValuation]
}

object TradeService {

  /** Item lines default their account and price from the item; free lines need both. */
  final case class NewLine(
      itemId: Option[ItemId],
      description: Option[Body],
      quantity: Quantity,
      unitPriceMinor: Option[PriceMinor],
      accountId: Option[AccountId],
      taxRateId: Option[TaxRateId]
  )

  /** Issued in the contact's currency; `dueDate` defaults to 30 days after `date`. */
  final case class NewDocument(
      contactId: ContactId,
      date: LocalDate,
      dueDate: Option[LocalDate],
      reference: Option[JournalRef],
      memo: Option[Body],
      exchangeRate: Option[ExchangeRate],
      lines: List[NewLine]
  )

  final case class NewAllocation(documentId: DocumentId, amountMinor: PriceMinor)

  /** `accountId` is the cash, bank or card account the money moved through. */
  final case class NewSettlement(
      contactId: ContactId,
      date: LocalDate,
      accountId: AccountId,
      reference: Option[JournalRef],
      memo: Option[Body],
      exchangeRate: Option[ExchangeRate],
      allocations: List[NewAllocation]
  )

  /**
   * Positive `quantity` adds stock at `valueMinor` (default: quantity x item
   * cost price); negative removes it at moving average cost. The offset goes
   * to `accountId` (default: Inventory adjustments).
   */
  final case class NewAdjustment(
      itemId: ItemId,
      date: LocalDate,
      quantity: BigDecimal,
      valueMinor: Option[PriceMinor],
      accountId: Option[AccountId],
      reason: Body
  )

  final case class DocumentSummary(document: Document, paidMinor: AmountMinor, status: DocumentStatus)

  def controlAccount(kind: DocumentKind): AccountId = kind match {
    case DocumentKind.Invoice => SystemAccounts.Receivables.id
    case DocumentKind.Bill    => SystemAccounts.Payables.id
  }

  def taxAccount(kind: DocumentKind): AccountId = kind match {
    case DocumentKind.Invoice => SystemAccounts.TaxPayable.id
    case DocumentKind.Bill    => SystemAccounts.InputTax.id
  }

  private def settlementSeries(kind: DocumentKind): String = kind match {
    case DocumentKind.Invoice => "receipt"
    case DocumentKind.Bill    => "bill_payment"
  }

  /** Resolves submitted lines against the catalogue: accounts, prices, tax and amounts. */
  def resolveLines(
      kind: DocumentKind,
      lines: List[NewLine],
      items: Map[ItemId, Item],
      rates: Map[TaxRateId, TaxRate]
  ): Either[LedgerError, List[DocumentLine]] =
    lines.zipWithIndex.traverse { case (l, i) =>
      val no = i + 1
      for {
        item    <- l.itemId.traverse(id =>
                     items.get(id).toRight(LedgerError.UnknownItem(id)).flatMap(it => Either.cond(it.active, it, LedgerError.InactiveItem(it.sku)))
                   )
        account <- item match {
                     case Some(it) if it.kind == ItemKind.Inventory =>
                       Right(if (kind == DocumentKind.Invoice) it.incomeAccountId else it.inventoryAccountId.getOrElse(SystemAccounts.Inventory.id))
                     case Some(it) =>
                       Right(l.accountId.getOrElse(if (kind == DocumentKind.Invoice) it.incomeAccountId else it.expenseAccountId))
                     case None =>
                       l.accountId.toRight(LedgerError.LineAccountRequired(no))
                   }
        price   <- l.unitPriceMinor
                     .orElse(item.flatMap(it => if (kind == DocumentKind.Invoice) it.sellPriceMinor else it.costPriceMinor))
                     .toRight(LedgerError.LinePriceRequired(no))
        rate    <- l.taxRateId.traverse(id =>
                     rates.get(id).toRight(LedgerError.UnknownTaxRate(id)).flatMap(r => Either.cond(r.active, r, LedgerError.InactiveTaxRate(r.code)))
                   )
      } yield {
        val amount = Trade.lineAmount(l.quantity.value, price.value)
        val tax    = Trade.lineTax(amount, rate.map(_.rateBps))
        DocumentLine(no, l.itemId, l.description, l.quantity, price, account, l.taxRateId, rate.map(_.rateBps),
                     AmountMinor.assume(amount), AmountMinor.assume(tax))
      }
    }

  private def contactFor(c: Option[Contact], id: ContactId, kind: ContactKind): Either[LedgerError, Contact] =
    c match {
      case None                       => Left(LedgerError.UnknownContact(id))
      case Some(x) if x.kind != kind  => Left(LedgerError.ContactKindMismatch(id, kind.key))
      case Some(x) if !x.active       => Left(LedgerError.InactiveContact(id))
      case Some(x)                    => Right(x)
    }

  def make[F[_]: Async](
      db: Db[F],
      posting: Posting[F],
      journals: Journals[F],
      contactRepo: Contacts[F],
      itemRepo: Items[F],
      taxRateRepo: TaxRates[F],
      documentRepo: Documents[F],
      settlementRepo: Settlements[F]
  ): TradeService[F] = new TradeService[F] {

    private type R[A] = EitherT[F, LedgerError, A]
    private def lift[A](fa: F[A]): R[A]                     = EitherT.liftF(fa)
    private def fail[A](err: LedgerError): R[A]             = EitherT.leftT[F, A](err)
    private def check(ok: Boolean, err: => LedgerError): R[Unit] = EitherT.cond[F](ok, (), err)
    private val unit: R[Unit]                               = EitherT.rightT[F, LedgerError](())

    private val base = posting.baseCurrency

    // ---- documents ----

    def createDocument(kind: DocumentKind, b: NewDocument, by: UserId): F[Either[LedgerError, DocumentDetails]] =
      (contactRepo.find(b.contactId), itemRepo.list(), taxRateRepo.list(), posting.chart).tupled.flatMap {
        case (contact, itemList, rateList, chart) =>
          val itemMap = itemList.map(i => i.id -> i).toMap
          val due     = b.dueDate.getOrElse(b.date.plusDays(30))
          val prepared = for {
            c     <- contactFor(contact, b.contactId, kind.contactKind)
            _     <- Either.cond(b.lines.nonEmpty, (), LedgerError.NoLines)
            _     <- Either.cond(!due.isBefore(b.date), (), LedgerError.DueBeforeIssue(b.date, due))
            lines <- resolveLines(kind, b.lines, itemMap, rateList.map(r => r.id -> r).toMap)
            _     <- Either.cond(lines.map(l => l.amountMinor.value + l.taxMinor.value).sum > 0L, (), LedgerError.ZeroTotal)
          } yield (c, lines)

          (for {
            (c, lines) <- EitherT.fromEither[F](prepared)
            rate       <- EitherT(posting.rate(c.currency, b.exchangeRate, b.date))
            id         <- lift(Async[F].delay(DocumentId.assume(UUID.randomUUID())))
            now        <- lift(Async[F].delay(Instant.now()))
            details    <- EitherT(db.txE(s => issue(s, kind, id, c, b, due, rate, lines, itemMap, chart, by, now)))
          } yield details).value
      }

    private def issue(
        s: Session[F],
        kind: DocumentKind,
        id: DocumentId,
        c: Contact,
        b: NewDocument,
        due: LocalDate,
        rate: ExchangeRate,
        lines: List[DocumentLine],
        itemMap: Map[ItemId, Item],
        chart: Map[AccountId, Account],
        by: UserId,
        now: Instant
    ): R[DocumentDetails] = {
      val subtotal = lines.map(_.amountMinor.value: Long).sum
      val tax      = lines.map(_.taxMinor.value: Long).sum
      for {
        n      <- lift(documentRepo.nextNumber(s, kind.key))
        number  = f"${kind.prefix}-$n%06d"
        header <- lift(posting.header(b.date, JournalRef.option(number), b.memo, c.currency, rate,
                                      kind.documentSource, Some(id.value), Some(by)))
        entry  <- posting.postIn(s, header, Trade.postingLines(kind, c.id, lines, controlAccount(kind), taxAccount(kind)), chart)
        doc     = Document(id, kind, number, c.id, b.date, due, c.currency, rate, b.reference, b.memo,
                           AmountMinor.assume(subtotal), AmountMinor.assume(tax), PriceMinor.assume(subtotal + tax),
                           entry.lines.head.baseAmountMinor, entry.header.id, None, Some(by), now)
        _      <- lift(documentRepo.insert(s, doc, lines))
        _      <- moveStock(s, doc, lines, entry, itemMap, chart, by)
      } yield DocumentDetails(doc, lines, AmountMinor.assume(0L), DocumentStatus.Open)
    }

    /** Bills receive stock at the lines' booked base value; invoices issue it at moving average and post COGS. */
    private def moveStock(
        s: Session[F],
        doc: Document,
        lines: List[DocumentLine],
        entry: JournalEntry,
        itemMap: Map[ItemId, Item],
        chart: Map[AccountId, Account],
        by: UserId
    ): R[Unit] = {
      val stocked = lines.flatMap(l => l.itemId.flatMap(itemMap.get).filter(_.kind == ItemKind.Inventory).map(l -> _))
      if (stocked.isEmpty) unit
      else lift(itemRepo.lockStock(s, stocked.map(_._2.id))).flatMap { stocks =>
        doc.kind match {
          case DocumentKind.Bill =>
            val bases = Trade.lineBases(entry.lines, lines)
            stocked.foldLeftM(stocks) { case (st, (l, it)) =>
              val value = bases.getOrElse(l.lineNo, 0L)
              val after = st(it.id).add(l.quantity.value, value)
              lift(itemRepo.move(s, InventoryMove(it.id, doc.date, l.quantity.value, value, JournalSource.Bill, doc.id.value), after))
                .as(st.updated(it.id, after))
            }.void

          case DocumentKind.Invoice =>
            for {
              issued <- stocked.foldLeftM((stocks, List.empty[(Item, Long)])) { case ((st, acc), (l, it)) =>
                          val stock = st(it.id)
                          val qty   = l.quantity.value
                          if (stock.quantity < qty) fail(LedgerError.InsufficientStock(it.sku, stock.quantity, qty))
                          else {
                            val cost  = stock.costOf(qty)
                            val after = stock.add(-qty, -cost)
                            lift(itemRepo.move(s, InventoryMove(it.id, doc.date, -qty, -cost, JournalSource.InventoryIssue, doc.id.value), after))
                              .as((st.updated(it.id, after), acc :+ (it -> cost)))
                          }
                        }
              cogs    = issued._2.flatMap { case (it, cost) =>
                          Ledger.line(it.expenseAccountId, Side.Debit, cost, None).toList ++
                            Ledger.line(it.inventoryAccountId.getOrElse(SystemAccounts.Inventory.id), Side.Credit, cost, None).toList
                        }
              _      <- if (cogs.isEmpty) unit
                        else lift(posting.header(doc.date, JournalRef.option(doc.number), None, base, ExchangeRate.One,
                                                 JournalSource.InventoryIssue, Some(doc.id.value), Some(by)))
                               .flatMap(h => posting.postIn(s, h, cogs, chart)).void
            } yield ()
        }
      }
    }

    def document(id: DocumentId): F[Option[DocumentDetails]] =
      documentRepo.find(id).flatMap(_.traverse { d =>
        (documentRepo.lines(id), documentRepo.paid(id)).mapN { (lines, paid) =>
          DocumentDetails(d, lines, AmountMinor.assume(paid._1), Trade.status(d, paid._1))
        }
      })

    def documents(kind: DocumentKind, from: LocalDate, to: LocalDate, contact: Option[ContactId], limit: Int): F[List[DocumentSummary]] =
      documentRepo.list(kind, from, to, contact, limit).map(_.map { case (d, paid) =>
        DocumentSummary(d, AmountMinor.assume(paid), Trade.status(d, paid))
      })

    def voidDocument(id: DocumentId, date: LocalDate, by: UserId): F[Either[LedgerError, DocumentDetails]] =
      (posting.chart, itemRepo.list()).tupled.flatMap { case (chart, itemList) =>
        db.txE { s =>
          for {
            doc   <- EitherT.fromOptionF(documentRepo.findForUpdate(s, id), LedgerError.DocumentNotFound(id): LedgerError)
            _     <- doc.voidedOn.fold(unit)(on => fail(LedgerError.AlreadyVoided(on)))
            _     <- check(!date.isBefore(doc.date), LedgerError.VoidBeforeIssue(doc.date, date))
            paid  <- lift(documentRepo.paidIn(s, id))
            _     <- check(paid._1 == 0L, LedgerError.DocumentHasPayments)
            _     <- posting.reverseIn(s, doc.entryId, date, Some(by), chart)
            _     <- doc.kind match {
                       case DocumentKind.Invoice =>
                         for {
                           cogs <- lift(journals.bySourceIn(s, JournalSource.InventoryIssue, doc.id.value))
                           _    <- cogs.traverse_(e => posting.reverseIn(s, e, date, Some(by), chart))
                           _    <- restock(s, doc, JournalSource.InventoryIssue, date, itemList)
                         } yield ()
                       case DocumentKind.Bill =>
                         restock(s, doc, JournalSource.Bill, date, itemList)
                     }
            _     <- lift(documentRepo.markVoided(s, id, date))
            lines <- lift(documentRepo.linesIn(s, id))
          } yield DocumentDetails(doc.copy(voidedOn = Some(date)), lines, AmountMinor.assume(0L), DocumentStatus.Void)
        }
      }

    /** Undoes the stock moves `doc` made. A bill's stock must still be on hand at its received value. */
    private def restock(s: Session[F], doc: Document, source: JournalSource, date: LocalDate, itemList: List[Item]): R[Unit] =
      lift(itemRepo.movesFor(s, source, doc.id.value)).flatMap { moves =>
        if (moves.isEmpty) unit
        else lift(itemRepo.lockStock(s, moves.map(_.itemId))).flatMap { stocks =>
          moves.foldLeftM(stocks) { (st, m) =>
            val stock = st(m.itemId)
            val after = stock.add(-m.quantity, -m.valueMinor)
            val sku   = itemList.find(_.id == m.itemId).map(_.sku).getOrElse(Sku.assume("?"))
            if (after.quantity < 0 || after.valueMinor < 0L || (after.quantity == 0 && after.valueMinor != 0L))
              fail(LedgerError.StockAlreadyConsumed(sku, stock.quantity, m.quantity))
            else
              lift(itemRepo.move(s, InventoryMove(m.itemId, date, -m.quantity, -m.valueMinor, JournalSource.Reversal, doc.id.value), after))
                .as(st.updated(m.itemId, after))
          }.void
        }
      }

    // ---- settlements ----

    def settle(kind: DocumentKind, b: NewSettlement, by: UserId): F[Either[LedgerError, SettlementDetails]] =
      (contactRepo.find(b.contactId), posting.chart).tupled.flatMap { case (contact, chart) =>
        val ids = b.allocations.map(_.documentId)
        val prepared = for {
          c    <- contactFor(contact, b.contactId, kind.contactKind)
          _    <- Either.cond(b.allocations.nonEmpty, (), LedgerError.NoLines)
          _    <- ids.diff(ids.distinct).headOption.map(d => LedgerError.AllocationMismatch(d, "allocated more than once")).toLeft(())
          acct <- chart.get(b.accountId).toRight(LedgerError.UnknownAccount(b.accountId))
          _    <- Either.cond(acct.accountType.isMoney, (), LedgerError.WrongAccountType(acct.code, "cash, bank or credit card"))
        } yield c

        (for {
          c       <- EitherT.fromEither[F](prepared)
          rate    <- EitherT(posting.rate(c.currency, b.exchangeRate, b.date))
          id      <- lift(Async[F].delay(SettlementId.assume(UUID.randomUUID())))
          now     <- lift(Async[F].delay(Instant.now()))
          details <- EitherT(db.txE(s => record(s, kind, id, c, b, rate, chart, by, now)))
        } yield details).value
      }

    private def record(
        s: Session[F],
        kind: DocumentKind,
        id: SettlementId,
        c: Contact,
        b: NewSettlement,
        rate: ExchangeRate,
        chart: Map[AccountId, Account],
        by: UserId,
        now: Instant
    ): R[SettlementDetails] =
      for {
        allocs <- b.allocations.sortBy(_.documentId.value).traverse(a => allocate(s, kind, c, b.date, a))
        n      <- lift(documentRepo.nextNumber(s, settlementSeries(kind)))
        number  = f"${kind.settlementPrefix}-$n%06d"
        total   = allocs.map(_.amountMinor.value: Long).sum
        header <- lift(posting.header(b.date, JournalRef.option(number), b.memo, c.currency, rate,
                                      kind.settlementSource, Some(id.value), Some(by)))
        lines   = JournalLine(b.accountId, kind.controlSide, PriceMinor.assume(total), None) ::
                    allocs.map(a => JournalLine(controlAccount(kind), kind.controlSide.opposite, a.amountMinor, Some(c.id)))
        entry  <- posting.postIn(s, header, lines, chart)
        booked  = allocs.zip(entry.lines.drop(1)).map { case (a, p) => a.copy(baseMinor = p.baseAmountMinor) }
        diff    = booked.map(a => a.baseMinor.value - a.documentBaseMinor.value).sum
        fx     <- if (diff == 0L) EitherT.rightT[F, LedgerError](Option.empty[JournalEntryId])
                  else lift(posting.header(b.date, JournalRef.option(number), None, base, ExchangeRate.One,
                                           JournalSource.FxRealized, Some(id.value), Some(by)))
                         .flatMap(h => posting.postIn(s, h, Trade.fxLines(kind, c.id, diff, controlAccount(kind),
                                                                          SystemAccounts.ExchangeGainLoss.id), chart))
                         .map(e => Some(e.header.id))
        st      = Settlement(id, kind, number, c.id, b.date, c.currency, rate, b.accountId, b.reference, b.memo,
                             PriceMinor.assume(total), entry.header.id, fx, None, Some(by), now)
        _      <- lift(settlementRepo.insert(s, st, booked))
      } yield SettlementDetails(st, booked)

    /** Locks the document and checks the allocation fits; the base at settlement rate is filled in after posting. */
    private def allocate(s: Session[F], kind: DocumentKind, c: Contact, date: LocalDate, a: NewAllocation): R[Allocation] =
      for {
        d    <- EitherT.fromOptionF(documentRepo.findForUpdate(s, a.documentId), LedgerError.DocumentNotFound(a.documentId): LedgerError)
        _    <- check(d.kind == kind && d.contactId == c.id, LedgerError.AllocationMismatch(d.id, s"not a ${kind.key} of this contact"))
        _    <- check(d.voidedOn.isEmpty, LedgerError.AllocationMismatch(d.id, "document is void"))
        _    <- check(!date.isBefore(d.date), LedgerError.AllocationMismatch(d.id, "settlement predates the document"))
        paid <- lift(documentRepo.paidIn(s, d.id))
        open  = d.totalMinor.value - paid._1
        _    <- check(a.amountMinor.value <= open, LedgerError.OverAllocation(d.id, open, a.amountMinor.value))
      } yield Allocation(d.id, a.amountMinor, AmountMinor.assume(0L),
                         AmountMinor.assume(Trade.documentBase(d, paid._1, paid._2, a.amountMinor.value)))

    def settlement(id: SettlementId): F[Option[SettlementDetails]] = settlementRepo.find(id)

    def settlements(kind: DocumentKind, from: LocalDate, to: LocalDate, limit: Int): F[List[Settlement]] =
      settlementRepo.list(kind, from, to, limit)

    def voidSettlement(id: SettlementId, date: LocalDate, by: UserId): F[Either[LedgerError, SettlementDetails]] =
      posting.chart.flatMap { chart =>
        db.txE { s =>
          for {
            sd <- EitherT.fromOptionF(settlementRepo.findForUpdate(s, id), LedgerError.SettlementNotFound(id): LedgerError)
            st  = sd.settlement
            _  <- st.voidedOn.fold(unit)(on => fail(LedgerError.AlreadyVoided(on)))
            _  <- check(!date.isBefore(st.date), LedgerError.VoidBeforeIssue(st.date, date))
            _  <- posting.reverseIn(s, st.entryId, date, Some(by), chart)
            _  <- st.fxEntryId.traverse_(e => posting.reverseIn(s, e, date, Some(by), chart))
            _  <- lift(settlementRepo.markVoided(s, id, date))
          } yield sd.copy(settlement = st.copy(voidedOn = Some(date)))
        }
      }

    // ---- inventory adjustments ----

    def adjustStock(b: NewAdjustment, by: UserId): F[Either[LedgerError, InventoryAdjustment]] =
      (itemRepo.list(), posting.chart).tupled.flatMap { case (itemList, chart) =>
        val prepared = for {
          it <- itemList.find(_.id == b.itemId).toRight(LedgerError.UnknownItem(b.itemId))
          _  <- Either.cond(it.active, (), LedgerError.InactiveItem(it.sku))
          _  <- Either.cond(it.kind == ItemKind.Inventory, (), LedgerError.NotAnInventoryItem(it.sku))
          _  <- Either.cond(b.quantity != 0, (), LedgerError.ZeroTotal)
        } yield it

        (for {
          it  <- EitherT.fromEither[F](prepared)
          id  <- lift(Async[F].delay(AdjustmentId.assume(UUID.randomUUID())))
          now <- lift(Async[F].delay(Instant.now()))
          adj <- EitherT(db.txE(s => adjust(s, it, id, b, chart, by, now)))
        } yield adj).value
      }

    private def adjust(
        s: Session[F],
        it: Item,
        id: AdjustmentId,
        b: NewAdjustment,
        chart: Map[AccountId, Account],
        by: UserId,
        now: Instant
    ): R[InventoryAdjustment] =
      for {
        stock  <- lift(itemRepo.lockStock(s, List(it.id))).map(_(it.id))
        value  <- if (b.quantity > 0)
                    EitherT.fromEither[F](
                      b.valueMinor.map(v => v.value: Long)
                        .orElse(it.costPriceMinor.map(c => Trade.lineAmount(b.quantity, c.value)))
                        .toRight(LedgerError.LinePriceRequired(1): LedgerError)
                    )
                  else if (stock.quantity < -b.quantity) fail(LedgerError.InsufficientStock(it.sku, stock.quantity, -b.quantity))
                  else EitherT.rightT[F, LedgerError](-stock.costOf(-b.quantity))
        offset  = b.accountId.getOrElse(SystemAccounts.InventoryAdjustments.id)
        lines   = Ledger.line(it.inventoryAccountId.getOrElse(SystemAccounts.Inventory.id), Side.Debit, value, None).toList ++
                    Ledger.line(offset, Side.Credit, value, None).toList
        header <- lift(posting.header(b.date, None, Some(b.reason), base, ExchangeRate.One,
                                      JournalSource.InventoryAdjustment, Some(id.value), Some(by)))
        entry  <- posting.postIn(s, header, lines, chart)
        _      <- lift(itemRepo.move(s, InventoryMove(it.id, b.date, b.quantity, value, JournalSource.InventoryAdjustment, id.value),
                                     stock.add(b.quantity, value)))
        adj     = InventoryAdjustment(id, it.id, b.date, b.quantity, value, offset, b.reason, entry.header.id, Some(by), now)
        _      <- lift(itemRepo.insertAdjustment(s, adj))
      } yield adj

    def adjustments(limit: Int): F[List[InventoryAdjustment]] = itemRepo.adjustments(limit)

    // ---- reports ----

    def aging(kind: DocumentKind, asOf: LocalDate): F[TradeReports.Aging] =
      (contactRepo.list(Some(kind.contactKind)), documentRepo.openItems(kind, asOf))
        .mapN(TradeReports.aging(kind, asOf, base, _, _))

    def salesTax(from: LocalDate, to: LocalDate): F[Either[LedgerError, TradeReports.SalesTax]] =
      LedgerService.period(from, to).traverse(_ =>
        (taxRateRepo.list(), documentRepo.taxActivity(from, to)).mapN(TradeReports.salesTax(from, to, base, _, _))
      )

    def inventoryValuation(asOf: LocalDate): F[TradeReports.InventoryValuation] =
      (itemRepo.list(), itemRepo.valuation(asOf)).mapN(TradeReports.inventoryValuation(asOf, base, _, _))
  }
}
