package wealth.portfolio.domain

import wealth.domain.*

import java.time.LocalDate

/** An open tax lot; `costBase` is the cost in base currency at the acquisition-date rate. */
final case class Lot(activityId: ActivityId, acquiredOn: LocalDate, quantity: BigDecimal, cost: BigDecimal, costBase: BigDecimal)

final case class Position(assetId: AssetId, currency: CurrencyCode, lots: Vector[Lot], lastTradePrice: Option[BigDecimal]) {
  def quantity: BigDecimal = lots.map(_.quantity).sum
  def cost: BigDecimal     = lots.map(_.cost).sum
  def costBase: BigDecimal = lots.map(_.costBase).sum
  def averageCost: Option[BigDecimal] = Option.when(quantity > 0)(cost / quantity)
}

/** A FIFO disposal slice. Proceeds are net of the selling fee. */
final case class Disposal(
    activityId: ActivityId,
    accountId: InvestmentAccountId,
    assetId: AssetId,
    date: LocalDate,
    quantity: BigDecimal,
    proceeds: BigDecimal,
    cost: BigDecimal,
    currency: CurrencyCode,
    proceedsBase: BigDecimal,
    costBase: BigDecimal
) {
  def realized: BigDecimal     = proceeds - cost
  def realizedBase: BigDecimal = proceedsBase - costBase
}

final case class IncomeEvent(
    activityId: ActivityId,
    accountId: InvestmentAccountId,
    assetId: Option[AssetId],
    kind: ActivityType,
    date: LocalDate,
    currency: CurrencyCode,
    amount: BigDecimal,
    amountBase: BigDecimal
)

/**
 * Money crossing the account boundary, in base currency (positive in).
 * `counterAccount` is set for transfers, which only count as flows when the
 * other side is outside the measured scope.
 */
final case class Flow(date: LocalDate, amountBase: BigDecimal, transfer: Boolean, counterAccount: Option[InvestmentAccountId])

/** An account's state after replaying its activities. */
final case class Book(
    accountId: InvestmentAccountId,
    positions: Map[AssetId, Position],
    cash: Map[CurrencyCode, BigDecimal],
    disposals: Vector[Disposal],
    income: Vector[IncomeEvent],
    flows: Vector[Flow]
) {
  def openPositions: List[Position] = positions.values.filter(_.quantity > 0).toList

  /** External contributions to date: deposits less withdrawals, plus external transfers. */
  def netContribution: BigDecimal = flows.filterNot(f => f.transfer && f.counterAccount.isDefined).map(_.amountBase).sum
}

object Book {
  def empty(accountId: InvestmentAccountId): Book = Book(accountId, Map.empty, Map.empty, Vector.empty, Vector.empty, Vector.empty)
}

/** Replays activities into positions, cash, realised gains, income and flows. */
object Engine {

  final case class Context(assets: Map[AssetId, Asset], prices: PriceTable, fx: FxTable, base: CurrencyCode)

  /** Activities in the order they are applied: by date, then as entered. */
  def ordered(activities: List[Activity]): List[Activity] =
    activities.sortBy(a => (a.date, a.createdAt, a.id.value))

  def replay(accountId: InvestmentAccountId, activities: List[Activity], ctx: Context): Either[PortfolioError, Book] =
    ordered(activities).foldLeft[Either[PortfolioError, Book]](Right(Book.empty(accountId)))((acc, a) => acc.flatMap(step(_, a, ctx)))

  /** Checks an activity's shape before it is applied. */
  def validate(a: Activity, assets: Map[AssetId, Asset]): Either[PortfolioError, Unit] = {
    def need(ok: Boolean, msg: => String) = Either.cond(ok, (), PortfolioError.InvalidActivity(s"${a.kind.key}: $msg"))
    val qty    = a.quantity.getOrElse(BigDecimal(0))
    val amount = a.amount.getOrElse(BigDecimal(0))
    for {
      asset <- a.assetId.traverseAsset(assets)
      _     <- need(!a.kind.needsAsset || asset.isDefined, "an asset is required")
      _     <- asset.fold(Right(()))(as => Either.cond(as.currency == a.currency, (), PortfolioError.CurrencyMismatch(as.currency, a.currency)))
      _     <- need(a.fee >= 0, "fee must not be negative")
      _     <- a.kind match {
                 case ActivityType.Buy | ActivityType.Sell =>
                   need(qty > 0 && a.unitPrice.exists(_ >= 0), "needs a positive quantity and a unit price")
                 case ActivityType.TransferIn | ActivityType.TransferOut =>
                   if (asset.isDefined) need(qty > 0 && a.unitPrice.exists(_ >= 0), "an asset transfer needs a positive quantity and a unit cost")
                   else need(amount > 0, "a cash transfer needs a positive amount")
                 case ActivityType.Split      => need(amount > 0, "needs a positive split ratio in amount")
                 case ActivityType.Adjustment => need(qty != 0, "needs a non-zero quantity change")
                 case ActivityType.Fee        => need(amount > 0 || a.fee > 0, "needs an amount")
                 case _                       => need(amount > 0, "needs a positive amount")
               }
    } yield ()
  }

  def step(book: Book, a: Activity, ctx: Context): Either[PortfolioError, Book] =
    validate(a, ctx.assets).flatMap { _ =>
      val qty    = a.quantity.getOrElse(BigDecimal(0))
      val price  = a.unitPrice.getOrElse(BigDecimal(0))
      val amount = a.amount.getOrElse(BigDecimal(0))
      def toBase(x: BigDecimal) = ctx.fx.convert(x, a.currency, ctx.base, a.date)
      def cash(delta: BigDecimal) = book.cash.updated(a.currency, book.cash.getOrElse(a.currency, BigDecimal(0)) + delta)
      def flow(x: BigDecimal, transfer: Boolean) =
        toBase(x).map(b => book.flows :+ Flow(a.date, b, transfer, if (transfer && !a.external) a.counterAccountId else None))

      a.kind match {
        case ActivityType.Buy =>
          val cost = qty * price + a.fee
          for {
            costBase <- toBase(cost)
            pos       = position(book, a)
          } yield book.copy(
            positions = book.positions.updated(pos.assetId, pos.copy(lots = pos.lots :+ Lot(a.id, a.date, qty, cost, costBase), lastTradePrice = Some(price))),
            cash      = cash(-cost)
          )

        case ActivityType.Sell =>
          val proceeds = qty * price - a.fee
          for {
            taken        <- take(book, a, qty, ctx)
            (pos, slices) = taken
            proceedsBase <- toBase(proceeds)
            disposals     = allocateProceeds(book.accountId, a, slices, proceeds, proceedsBase)
          } yield book.copy(
            positions = book.positions.updated(pos.assetId, pos.copy(lastTradePrice = Some(price))),
            cash      = cash(proceeds),
            disposals = book.disposals ++ disposals
          )

        case ActivityType.Dividend | ActivityType.Interest =>
          toBase(amount).map(b => book.copy(
            cash   = cash(amount - a.fee),
            income = book.income :+ IncomeEvent(a.id, book.accountId, a.assetId, a.kind, a.date, a.currency, amount, b)
          ))

        case ActivityType.Deposit    => flow(amount, transfer = false).map(fs => book.copy(cash = cash(amount - a.fee), flows = fs))
        case ActivityType.Withdrawal => flow(-amount, transfer = false).map(fs => book.copy(cash = cash(-amount - a.fee), flows = fs))

        case ActivityType.TransferIn if a.assetId.isDefined =>
          val cost   = qty * price
          val marked = ctx.prices.close(a.assetId.get, a.date).getOrElse(price) * qty
          for {
            costBase <- toBase(cost)
            fs       <- flow(marked, transfer = true)
            pos       = position(book, a)
          } yield book.copy(positions = book.positions.updated(pos.assetId, pos.copy(lots = pos.lots :+ Lot(a.id, a.date, qty, cost, costBase))),
                            cash = cash(-a.fee), flows = fs)

        case ActivityType.TransferOut if a.assetId.isDefined =>
          val marked = ctx.prices.close(a.assetId.get, a.date).getOrElse(price) * qty
          for {
            taken    <- take(book, a, qty, ctx)
            (pos, _)  = taken
            fs       <- flow(-marked, transfer = true)
          } yield book.copy(positions = book.positions.updated(pos.assetId, pos), cash = cash(-a.fee), flows = fs)

        case ActivityType.TransferIn  => flow(amount, transfer = true).map(fs => book.copy(cash = cash(amount - a.fee), flows = fs))
        case ActivityType.TransferOut => flow(-amount, transfer = true).map(fs => book.copy(cash = cash(-amount - a.fee), flows = fs))

        case ActivityType.Credit =>
          if (a.external) flow(amount, transfer = false).map(fs => book.copy(cash = cash(amount), flows = fs))
          else Right(book.copy(cash = cash(amount)))

        case ActivityType.Fee => Right(book.copy(cash = cash(-(if (amount > 0) amount else a.fee))))
        case ActivityType.Tax => Right(book.copy(cash = cash(-amount - a.fee)))

        case ActivityType.Split =>
          val pos = position(book, a)
          Right(book.copy(positions = book.positions.updated(pos.assetId,
            pos.copy(lots = pos.lots.map(l => l.copy(quantity = l.quantity * amount)), lastTradePrice = pos.lastTradePrice.map(_ / amount)))))

        case ActivityType.Adjustment =>
          if (qty > 0) {
            val cost = qty * price
            toBase(cost).map { costBase =>
              val pos = position(book, a)
              book.copy(positions = book.positions.updated(pos.assetId, pos.copy(lots = pos.lots :+ Lot(a.id, a.date, qty, cost, costBase))), cash = cash(-a.fee))
            }
          } else
            take(book, a, -qty, ctx).map { case (pos, slices) =>
              book.copy(positions = book.positions.updated(pos.assetId, pos), cash = cash(-a.fee),
                        disposals = book.disposals ++ allocateProceeds(book.accountId, a, slices, BigDecimal(0), BigDecimal(0)))
            }
      }
    }

  private def position(book: Book, a: Activity): Position = {
    val id = a.assetId.get
    book.positions.getOrElse(id, Position(id, a.currency, Vector.empty, None))
  }

  /** Removes `qty` FIFO; returns the remaining position and the lot slices taken. */
  private def take(book: Book, a: Activity, qty: BigDecimal, ctx: Context): Either[PortfolioError, (Position, Vector[Lot])] = {
    val pos = position(book, a)
    if (pos.quantity < qty)
      Left(PortfolioError.InsufficientQuantity(ctx.assets(pos.assetId).symbol, pos.quantity, qty, a.date))
    else {
      val (left, taken, _) = pos.lots.foldLeft((Vector.empty[Lot], Vector.empty[Lot], qty)) { case ((keep, out, need), lot) =>
        if (need <= 0) (keep :+ lot, out, need)
        else if (lot.quantity <= need) (keep, out :+ lot, need - lot.quantity)
        else {
          val share = need / lot.quantity
          val used  = Lot(lot.activityId, lot.acquiredOn, need, lot.cost * share, lot.costBase * share)
          val rest  = Lot(lot.activityId, lot.acquiredOn, lot.quantity - need, lot.cost - used.cost, lot.costBase - used.costBase)
          (keep :+ rest, out :+ used, BigDecimal(0))
        }
      }
      Right((pos.copy(lots = left), taken))
    }
  }

  /** Splits the proceeds across the lot slices by quantity; the last slice takes the remainder so they sum exactly. */
  private def allocateProceeds(
      accountId: InvestmentAccountId,
      a: Activity,
      slices: Vector[Lot],
      proceeds: BigDecimal,
      proceedsBase: BigDecimal
  ): Vector[Disposal] = {
    val total = slices.map(_.quantity).sum
    def split(amount: BigDecimal): Vector[BigDecimal] =
      if (slices.isEmpty || total == 0) slices.map(_ => BigDecimal(0))
      else {
        val init = slices.init.map(l => amount * l.quantity / total)
        init :+ (amount - init.sum)
      }
    slices.zip(split(proceeds)).zip(split(proceedsBase)).map { case ((l, p), pb) =>
      Disposal(a.id, accountId, a.assetId.get, a.date, l.quantity, p, l.cost, a.currency, pb, l.costBase)
    }
  }

  extension (id: Option[AssetId]) {
    private def traverseAsset(assets: Map[AssetId, Asset]): Either[PortfolioError, Option[Asset]] =
      id match {
        case None    => Right(None)
        case Some(x) => assets.get(x).map(Some(_)).toRight(PortfolioError.AssetNotFound(x))
      }
  }
}
