package moneystack.portfolio.service

import moneystack.domain.*
import moneystack.portfolio.domain.*
import moneystack.portfolio.db.{Accounts, Activities, Market, Plans}

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Investment tracking: accounts, the shared instrument catalogue and market
 * data, activities (validated by replaying the account), CSV import, and
 * the read models — holdings, value history, performance, income, realised
 * gains, allocation, rebalancing and contribution limits. Every read is
 * scoped to the owner and expressed in a base currency of the caller's choice.
 */
trait PortfolioService[F[_]] {
  def accounts(owner: UserId): F[List[InvestmentAccount]]
  def createAccount(owner: UserId, b: PortfolioService.NewAccount): F[InvestmentAccount]
  def setAccountActive(owner: UserId, id: InvestmentAccountId, active: Boolean): F[Option[InvestmentAccount]]
  def recordBalance(owner: UserId, s: BalanceSnapshot): F[Either[PortfolioError, BalanceSnapshot]]

  def assets: F[List[Asset]]
  def createAsset(b: PortfolioService.NewAsset): F[Either[PortfolioError, Asset]]
  def classify(asset: AssetId, taxonomy: Taxonomy, categories: List[(Title, Bps)]): F[Either[PortfolioError, List[Classification]]]
  def addQuotes(qs: List[Quote]): F[Either[PortfolioError, Int]]
  def addFxRates(rs: List[FxRate]): F[Either[PortfolioError, Int]]

  def activities(owner: UserId, account: Option[InvestmentAccountId], from: LocalDate, to: LocalDate): F[List[Activity]]
  def addActivity(owner: UserId, b: PortfolioService.NewActivity): F[Either[PortfolioError, Activity]]
  def updateActivity(owner: UserId, id: ActivityId, b: PortfolioService.NewActivity): F[Either[PortfolioError, Activity]]
  def deleteActivity(owner: UserId, id: ActivityId): F[Either[PortfolioError, Unit]]
  def importCsv(
      owner: UserId,
      account: InvestmentAccountId,
      csv: String,
      dryRun: Boolean
  ): F[Either[PortfolioError, PortfolioService.ImportReport]]

  def holdings(
      owner: UserId,
      accounts: Option[List[InvestmentAccountId]],
      asOf: LocalDate,
      base: CurrencyCode
  ): F[Either[PortfolioError, PortfolioService.Holdings]]
  def history(
      owner: UserId,
      accounts: Option[List[InvestmentAccountId]],
      from: LocalDate,
      to: LocalDate,
      base: CurrencyCode
  ): F[Either[PortfolioError, List[DailyValue]]]
  def performance(
      owner: UserId,
      accounts: Option[List[InvestmentAccountId]],
      from: LocalDate,
      to: LocalDate,
      base: CurrencyCode
  ): F[Either[PortfolioError, PortfolioService.PerformanceReport]]
  def income(
      owner: UserId,
      accounts: Option[List[InvestmentAccountId]],
      from: LocalDate,
      to: LocalDate,
      base: CurrencyCode
  ): F[Either[PortfolioError, IncomeSummary]]
  def realized(
      owner: UserId,
      accounts: Option[List[InvestmentAccountId]],
      from: LocalDate,
      to: LocalDate,
      base: CurrencyCode
  ): F[Either[PortfolioError, PortfolioService.RealizedReport]]

  /** Each account's total value on `asOf`, in base currency. */
  def accountValues(owner: UserId, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, Map[InvestmentAccountId, BigDecimal]]]

  def allocation(owner: UserId, taxonomy: String, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, List[Slice]]]
  def setTargets(owner: UserId, taxonomy: Taxonomy, targets: List[(Title, Bps)]): F[Either[PortfolioError, List[AllocationTarget]]]
  def drift(owner: UserId, taxonomy: Taxonomy, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, List[DriftRow]]]
  def rebalance(
      owner: UserId,
      taxonomy: Taxonomy,
      newCash: BigDecimal,
      mode: RebalanceMode,
      asOf: LocalDate,
      base: CurrencyCode
  ): F[Either[PortfolioError, List[RebalanceRow]]]

  def createLimit(owner: UserId, b: PortfolioService.NewLimit): F[Either[PortfolioError, ContributionLimit]]
  def deleteLimit(owner: UserId, id: LimitId): F[Either[PortfolioError, Unit]]
  def limits(owner: UserId, base: CurrencyCode): F[Either[PortfolioError, List[LimitProgress]]]
}

object PortfolioService {

  final case class NewAccount(name: Title, kind: AccountKind, tracking: Option[Tracking], currency: CurrencyCode, group: Option[Title])

  final case class NewAsset(symbol: Symbol, name: Title, kind: AssetKind, currency: CurrencyCode)

  /** `symbol` may stand in for `assetId`; an unknown symbol is an error (create the asset first). */
  final case class NewActivity(
      accountId: InvestmentAccountId,
      assetId: Option[AssetId],
      symbol: Option[Symbol],
      kind: ActivityType,
      date: LocalDate,
      quantity: Option[BigDecimal],
      unitPrice: Option[BigDecimal],
      amount: Option[BigDecimal],
      fee: Option[BigDecimal],
      currency: CurrencyCode,
      external: Option[Boolean],
      counterAccountId: Option[InvestmentAccountId],
      note: Option[Body]
  )

  final case class NewLimit(
      name: Title,
      year: Int,
      limitAmount: BigDecimal,
      currency: CurrencyCode,
      accountIds: List[InvestmentAccountId],
      startDate: Option[LocalDate],
      endDate: Option[LocalDate]
  )

  /** Unknown symbols in an import become new assets of kind `other` in the row's currency. */
  final case class ImportReport(rows: Int, imported: Int, duplicates: Int, createdAssets: List[Symbol], errors: List[String])

  final case class Holdings(
      asOf: LocalDate,
      currency: CurrencyCode,
      positions: List[HoldingView],
      cash: BigDecimal,
      investments: BigDecimal,
      total: BigDecimal,
      costBasis: BigDecimal,
      unrealized: BigDecimal
  )

  final case class PerformanceReport(currency: CurrencyCode, returns: Option[Returns], curve: List[ReturnPoint])

  final case class RealizedReport(
      currency: CurrencyCode,
      disposals: List[Disposal],
      proceedsBase: BigDecimal,
      costBase: BigDecimal,
      realizedBase: BigDecimal
  )

  private def money(x: BigDecimal) = x.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)

  def make[F[_]: Async](accountsRepo: Accounts[F], market: Market[F], activityRepo: Activities[F], plans: Plans[F]): PortfolioService[F] =
    new PortfolioService[F] {

      private type R[A] = EitherT[F, PortfolioError, A]
      private def lift[A](fa: F[A]): R[A] = EitherT.liftF(fa)

      // ---- accounts ----

      def accounts(owner: UserId): F[List[InvestmentAccount]] = accountsRepo.list(owner)

      def createAccount(owner: UserId, b: NewAccount): F[InvestmentAccount] =
        for {
          id  <- Async[F].delay(InvestmentAccountId.assume(UUID.randomUUID()))
          now <- Async[F].delay(Instant.now())
          a   <- accountsRepo.create(InvestmentAccount(
            id,
            owner,
            b.name,
            b.kind,
            b.tracking.getOrElse(Tracking.Activities),
            b.currency,
            b.group,
            active = true,
            now
          ))
        } yield a

      def setAccountActive(owner: UserId, id: InvestmentAccountId, active: Boolean): F[Option[InvestmentAccount]] =
        accountsRepo.setActive(owner, id, active)

      def recordBalance(owner: UserId, s: BalanceSnapshot): F[Either[PortfolioError, BalanceSnapshot]] =
        (for {
          a <- owned(owner, s.accountId)
          _ <- EitherT.cond[F](a.tracking == Tracking.Balances, (), PortfolioError.WrongTracking(a.id, "balances"): PortfolioError)
          _ <- lift(accountsRepo.upsertSnapshot(s))
        } yield s).value

      private def owned(owner: UserId, id: InvestmentAccountId): R[InvestmentAccount] =
        EitherT(accountsRepo.list(owner).map(_.find(_.id == id).toRight(PortfolioError.AccountNotFound(id))))

      // ---- market data ----

      def assets: F[List[Asset]] = market.assets()

      def createAsset(b: NewAsset): F[Either[PortfolioError, Asset]] =
        market.assetBySymbol(b.symbol).flatMap {
          case Some(_) => Async[F].pure(Left(PortfolioError.DuplicateSymbol(b.symbol)))
          case None    =>
            for {
              id  <- Async[F].delay(AssetId.assume(UUID.randomUUID()))
              now <- Async[F].delay(Instant.now())
              a   <- market.createAsset(Asset(id, b.symbol, b.name, b.kind, b.currency, now))
            } yield Right(a)
        }

      def classify(asset: AssetId, taxonomy: Taxonomy, categories: List[(Title, Bps)]): F[Either[PortfolioError, List[Classification]]] =
        market.assets().flatMap { all =>
          val total = categories.map(_._2.value).sum
          if (!all.exists(_.id == asset)) Async[F].pure(Left(PortfolioError.AssetNotFound(asset)))
          else if (total > 10_000 || categories.exists(_._2.value == 0))
            Async[F].pure(Left(PortfolioError.InvalidTargets("category weights must be positive and total at most 10000 bps")))
          else if (categories.map(_._1).distinct.size != categories.size)
            Async[F].pure(Left(PortfolioError.InvalidTargets("each category may appear once")))
          else {
            val cs = categories.map { case (c, w) => Classification(asset, taxonomy, c, w) }
            market.classify(asset, taxonomy, cs).as(Right(cs))
          }
        }

      def addQuotes(qs: List[Quote]): F[Either[PortfolioError, Int]] =
        market.assets().flatMap { all =>
          qs.map(_.assetId).find(id => !all.exists(_.id == id)) match {
            case Some(missing)                  => Async[F].pure(Left(PortfolioError.AssetNotFound(missing)))
            case None if qs.exists(_.close < 0) => Async[F].pure(Left(PortfolioError.InvalidInput("prices must not be negative")))
            case None                           => market.upsertQuotes(qs).as(Right(qs.size))
          }
        }

      def addFxRates(rs: List[FxRate]): F[Either[PortfolioError, Int]] =
        if (rs.exists(r => r.from == r.to || r.rate <= 0))
          Async[F].pure(Left(PortfolioError.InvalidInput("rates need two different currencies and a positive rate")))
        else market.upsertFx(rs).as(Right(rs.size))

      // ---- loading ----

      /** The owner's accounts (optionally a subset), their data, and the pricing context in `base`. */
      private def load(owner: UserId, subset: Option[List[InvestmentAccountId]], base: CurrencyCode): R[(Scope, Engine.Context)] =
        for {
          all    <- lift(accountsRepo.list(owner))
          chosen <- subset.fold(EitherT.rightT[F, PortfolioError](all))(ids =>
            ids.traverse(id => EitherT.fromOption[F](all.find(_.id == id), PortfolioError.AccountNotFound(id): PortfolioError))
          )
          acts    <- lift(activityRepo.forAccounts(chosen.map(_.id)))
          snaps   <- lift(accountsRepo.snapshots(chosen.map(_.id)))
          assetsL <- lift(market.assets())
          quotes  <- lift(market.quotes(acts.flatMap(_.assetId).distinct))
          rates   <- lift(market.fxRates())
        } yield (
          Scope(chosen, acts.groupBy(_.accountId), snaps.groupBy(_.accountId)),
          Engine.Context(assetsL.map(a => a.id -> a).toMap, PriceTable(quotes), FxTable(rates), base)
        )

      private def books(scope: Scope, ctx: Engine.Context, asOf: LocalDate): Either[PortfolioError, List[Book]] =
        scope.accounts.filter(_.tracking == Tracking.Activities)
          .traverse(a => Engine.replay(a.id, scope.activities.getOrElse(a.id, Nil).filter(!_.date.isAfter(asOf)), ctx))

      // ---- activities ----

      def activities(owner: UserId, account: Option[InvestmentAccountId], from: LocalDate, to: LocalDate): F[List[Activity]] =
        accountsRepo.list(owner).flatMap { all =>
          val ids = account.fold(all.map(_.id))(id => all.filter(_.id == id).map(_.id))
          activityRepo.forAccounts(ids).map(_.filter(a => !a.date.isBefore(from) && !a.date.isAfter(to)).sortBy(a =>
            (a.date, a.createdAt)
          ).reverse)
        }

      private def build(owner: UserId, id: ActivityId, b: NewActivity, createdAt: Instant): R[Activity] =
        for {
          acct <- owned(owner, b.accountId)
          _    <-
            EitherT.cond[F](acct.tracking == Tracking.Activities, (), PortfolioError.WrongTracking(acct.id, "activities"): PortfolioError)
          _     <- EitherT.cond[F](acct.active, (), PortfolioError.InvalidActivity("the account is inactive"): PortfolioError)
          _     <- b.counterAccountId.traverse_(c => owned(owner, c).void)
          asset <- (b.assetId, b.symbol) match {
            case (Some(a), _)    => EitherT.rightT[F, PortfolioError](Option(a))
            case (None, Some(s)) => EitherT.fromOptionF(
                market.assetBySymbol(s),
                PortfolioError.InvalidActivity(s"unknown symbol ${s.value}"): PortfolioError
              ).map(a => Option(a.id))
            case (None, None) => EitherT.rightT[F, PortfolioError](Option.empty[AssetId])
          }
        } yield Activity(
          id,
          b.accountId,
          asset,
          b.kind,
          b.date,
          b.quantity,
          b.unitPrice,
          b.amount,
          b.fee.getOrElse(BigDecimal(0)),
          b.currency,
          b.external.getOrElse(false),
          b.counterAccountId,
          b.note,
          None,
          createdAt
        )

      /** Replays the account with `change` applied, so an edit can't leave a later sale unbacked. */
      private def checkReplay(owner: UserId, account: InvestmentAccountId, change: List[Activity] => List[Activity]): R[Unit] =
        for {
          loaded <- load(owner, Some(List(account)), CurrencyCode.USD)
          (scope, ctx) = loaded
          // Validation is about quantities, not values, so missing FX rates must not block it.
          _ <- EitherT.fromEither[F](Engine.replay(
            account,
            change(scope.activities.getOrElse(account, Nil)),
            ctx.copy(fx = FxTable.unity)
          ).void)
        } yield ()

      def addActivity(owner: UserId, b: NewActivity): F[Either[PortfolioError, Activity]] =
        (for {
          id  <- lift(Async[F].delay(ActivityId.assume(UUID.randomUUID())))
          now <- lift(Async[F].delay(Instant.now()))
          a   <- build(owner, id, b, now)
          _   <- checkReplay(owner, a.accountId, _ :+ a)
          out <- lift(activityRepo.insert(a))
        } yield out).value

      def updateActivity(owner: UserId, id: ActivityId, b: NewActivity): F[Either[PortfolioError, Activity]] =
        (for {
          existing <- findActivity(owner, id)
          a        <- build(owner, id, b, existing.createdAt)
          _        <- EitherT.cond[F](
            a.accountId == existing.accountId,
            (),
            PortfolioError.InvalidActivity("an activity can't move between accounts"): PortfolioError
          )
          _   <- checkReplay(owner, a.accountId, _.map(x => if (x.id == id) a.copy(importKey = existing.importKey) else x))
          out <- EitherT.fromOptionF(
            activityRepo.replace(a.copy(importKey = existing.importKey)),
            PortfolioError.ActivityNotFound(id): PortfolioError
          )
        } yield out).value

      def deleteActivity(owner: UserId, id: ActivityId): F[Either[PortfolioError, Unit]] =
        (for {
          existing <- findActivity(owner, id)
          _        <- checkReplay(owner, existing.accountId, _.filterNot(_.id == id))
          _        <- lift(activityRepo.delete(id, existing.accountId))
        } yield ()).value

      private def findActivity(owner: UserId, id: ActivityId): R[Activity] =
        EitherT(accountsRepo.list(owner).flatMap(as => activityRepo.forAccounts(as.map(_.id)))
          .map(_.find(_.id == id).toRight(PortfolioError.ActivityNotFound(id))))

      def importCsv(owner: UserId, account: InvestmentAccountId, csv: String, dryRun: Boolean): F[Either[PortfolioError, ImportReport]] = {
        val parsed = CsvImport.parse(csv)
        (for {
          acct <- owned(owner, account)
          _    <-
            EitherT.cond[F](acct.tracking == Tracking.Activities, (), PortfolioError.WrongTracking(acct.id, "activities"): PortfolioError)
          _     <- EitherT.cond[F](parsed.errors.isEmpty, (), PortfolioError.ImportFailed(parsed.errors): PortfolioError)
          known <- lift(market.assets())
          symbols = parsed.rows.flatMap(r => r.symbol.map(_ -> r.currency)).distinctBy(_._1)
          missing = symbols.filterNot(s => known.exists(_.symbol == s._1))
          now     <- lift(Async[F].delay(Instant.now()))
          created <- if (dryRun) EitherT.rightT[F, PortfolioError](missing.map { case (s, c) =>
            Asset(AssetId.assume(UUID.nameUUIDFromBytes(s.value.getBytes)), s, Title.assume(s.value), AssetKind.Other, c, now)
          })
          else missing.traverse { case (s, c) => EitherT(createAsset(NewAsset(s, Title.assume(s.value), AssetKind.Other, c))) }
          bySymbol = (known ++ created).map(a => (a.symbol.value: String) -> a.id).toMap
          rows <- lift(parsed.rows.traverse(r =>
            Async[F].delay(ActivityId.assume(UUID.randomUUID())).map { id =>
              Activity(
                id,
                account,
                r.symbol.flatMap(s => bySymbol.get(s.value)),
                r.kind,
                r.date,
                r.quantity,
                r.unitPrice,
                r.amount,
                r.fee,
                r.currency,
                r.external,
                None,
                r.note,
                Some(r.key(account)),
                now
              )
            }
          ))
          existing <- lift(activityRepo.forAccounts(List(account)))
          fresh = rows.filterNot(r => existing.exists(_.importKey == r.importKey)).distinctBy(_.importKey)
          _        <- EitherT.fromEither[F](fresh.traverse_(a => Engine.validate(a, (known ++ created).map(x => x.id -> x).toMap)))
          _        <- if (dryRun) EitherT.rightT[F, PortfolioError](()) else checkReplay(owner, account, _ ++ fresh)
          inserted <- if (dryRun) EitherT.rightT[F, PortfolioError](fresh) else lift(activityRepo.insertImported(fresh))
        } yield ImportReport(parsed.rows.size, inserted.size, parsed.rows.size - inserted.size, missing.map(_._1), Nil)).value
      }

      // ---- read models ----

      def holdings(
          owner: UserId,
          accounts: Option[List[InvestmentAccountId]],
          asOf: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, Holdings]] =
        (for {
          loaded <- load(owner, accounts, base)
          (scope, ctx) = loaded
          bs  <- EitherT.fromEither[F](books(scope, ctx, asOf))
          hs  <- EitherT.fromEither[F](Valuation.holdings(bs, ctx, asOf))
          day <- EitherT.fromEither[F](Valuation.series(scope, ctx, asOf, asOf))
        } yield {
          val inv   = hs.map(_.marketValueBase).sum
          val total = day.lastOption.fold(BigDecimal(0))(_.total)
          Holdings(
            asOf,
            base,
            hs,
            money(total - inv),
            money(inv),
            money(total),
            money(hs.map(_.costBase).sum),
            money(hs.map(_.unrealizedBase).sum)
          )
        }).value

      def history(
          owner: UserId,
          accounts: Option[List[InvestmentAccountId]],
          from: LocalDate,
          to: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, List[DailyValue]]] =
        (for {
          _      <- period(from, to)
          loaded <- load(owner, accounts, base)
          series <- EitherT.fromEither[F](Valuation.series(loaded._1, loaded._2, from, to))
        } yield series).value

      def performance(
          owner: UserId,
          accounts: Option[List[InvestmentAccountId]],
          from: LocalDate,
          to: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, PerformanceReport]] =
        history(owner, accounts, from, to, base).map(_.map(s => PerformanceReport(base, Performance.measure(s), Performance.curve(s))))

      def income(
          owner: UserId,
          accounts: Option[List[InvestmentAccountId]],
          from: LocalDate,
          to: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, IncomeSummary]] =
        (for {
          _      <- period(from, to)
          loaded <- load(owner, accounts, base)
          bs     <- EitherT.fromEither[F](books(loaded._1, loaded._2, LocalDate.MAX))
        } yield Analytics.income(bs.flatMap(_.income), from, to, to)).value

      def realized(
          owner: UserId,
          accounts: Option[List[InvestmentAccountId]],
          from: LocalDate,
          to: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, RealizedReport]] =
        (for {
          _      <- period(from, to)
          loaded <- load(owner, accounts, base)
          bs     <- EitherT.fromEither[F](books(loaded._1, loaded._2, to))
        } yield {
          val ds = bs.flatMap(_.disposals).filter(d => !d.date.isBefore(from)).sortBy(_.date)
          RealizedReport(base, ds, money(ds.map(_.proceedsBase).sum), money(ds.map(_.costBase).sum), money(ds.map(_.realizedBase).sum))
        }).value

      def accountValues(
          owner: UserId,
          asOf: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, Map[InvestmentAccountId, BigDecimal]]] =
        (for {
          loaded <- load(owner, None, base)
          (scope, ctx) = loaded
          values <- EitherT.fromEither[F](scope.accounts.traverse { a =>
            Valuation.series(Scope(List(a), scope.activities, scope.snapshots), ctx, asOf, asOf)
              .map(s => a.id -> s.lastOption.fold(BigDecimal(0))(_.total))
          })
        } yield values.toMap).value

      // ---- allocation ----

      private def slices(owner: UserId, taxonomy: String, asOf: LocalDate, base: CurrencyCode): R[List[Slice]] =
        for {
          h  <- EitherT(holdings(owner, None, asOf, base))
          cs <- lift(market.classifications())
        } yield Analytics.allocation(h.positions, h.cash, taxonomy, cs)

      def allocation(owner: UserId, taxonomy: String, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, List[Slice]]] =
        slices(owner, taxonomy, asOf, base).value

      def setTargets(owner: UserId, taxonomy: Taxonomy, targets: List[(Title, Bps)]): F[Either[PortfolioError, List[AllocationTarget]]] =
        Analytics.validateTargets(targets.map { case (c, w) => (c.value: String) -> (w.value: Int) }) match {
          case Left(e)  => Async[F].pure(Left(e))
          case Right(_) =>
            targets.traverse { case (c, w) => Async[F].delay(AllocationTarget(TargetId.assume(UUID.randomUUID()), owner, taxonomy, c, w)) }
              .flatTap(plans.setTargets(owner, taxonomy, _)).map(Right(_))
        }

      def drift(owner: UserId, taxonomy: Taxonomy, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, List[DriftRow]]] =
        (for {
          s  <- slices(owner, taxonomy.value, asOf, base)
          ts <- lift(plans.targets(owner, taxonomy))
        } yield Analytics.drift(s, ts)).value

      def rebalance(
          owner: UserId,
          taxonomy: Taxonomy,
          newCash: BigDecimal,
          mode: RebalanceMode,
          asOf: LocalDate,
          base: CurrencyCode
      ): F[Either[PortfolioError, List[RebalanceRow]]] =
        (for {
          _  <- EitherT.cond[F](newCash >= 0, (), PortfolioError.InvalidInput("new cash must not be negative"): PortfolioError)
          s  <- slices(owner, taxonomy.value, asOf, base)
          ts <- lift(plans.targets(owner, taxonomy))
          _  <- EitherT.cond[F](ts.nonEmpty, (), PortfolioError.InvalidTargets(s"no targets set for ${taxonomy.value}"): PortfolioError)
        } yield Analytics.rebalance(s, ts, newCash, mode)).value

      // ---- contribution limits ----

      def createLimit(owner: UserId, b: NewLimit): F[Either[PortfolioError, ContributionLimit]] =
        (for {
          _ <- EitherT.cond[F](b.limitAmount > 0, (), PortfolioError.InvalidInput("the limit must be positive"): PortfolioError)
          _ <- EitherT.cond[F](b.accountIds.nonEmpty, (), PortfolioError.InvalidInput("a limit needs at least one account"): PortfolioError)
          _ <- b.accountIds.distinct.traverse_(owned(owner, _))
          id  <- lift(Async[F].delay(LimitId.assume(UUID.randomUUID())))
          out <- lift(plans.createLimit(ContributionLimit(
            id,
            owner,
            b.name,
            b.year,
            b.limitAmount,
            b.currency,
            b.accountIds.distinct,
            b.startDate,
            b.endDate
          )))
        } yield out).value

      def deleteLimit(owner: UserId, id: LimitId): F[Either[PortfolioError, Unit]] =
        plans.deleteLimit(owner, id).map(found => Either.cond(found, (), PortfolioError.LimitNotFound(id)))

      def limits(owner: UserId, base: CurrencyCode): F[Either[PortfolioError, List[LimitProgress]]] =
        (for {
          ls     <- lift(plans.limits(owner))
          loaded <- load(owner, None, base)
          bs     <- EitherT.fromEither[F](books(loaded._1, loaded._2, LocalDate.MAX))
          out    <- EitherT.fromEither[F](ls.traverse(Analytics.limitProgress(_, bs, loaded._2.fx, base)))
        } yield out).value

      private def period(from: LocalDate, to: LocalDate): R[Unit] =
        EitherT.cond[F](!from.isAfter(to), (), PortfolioError.InvalidPeriod(from, to): PortfolioError)
    }
}
