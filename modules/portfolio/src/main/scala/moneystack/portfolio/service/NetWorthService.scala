package moneystack.portfolio.service

import moneystack.domain.*
import moneystack.portfolio.domain.*
import moneystack.portfolio.db.{Accounts, Market, Wealth}

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate, YearMonth}
import java.util.UUID

/**
 * Everything owned less everything owed: investment accounts, property,
 * vehicles and other assets (at the owner's share), and amortising loans.
 */
trait NetWorthService[F[_]] {
  def altAssets(owner: UserId): F[List[AltAsset]]
  def createAltAsset(owner: UserId, b: NetWorthService.NewAltAsset): F[Either[PortfolioError, AltAsset]]
  def setAltAssetActive(owner: UserId, id: AltAssetId, active: Boolean): F[Option[AltAsset]]
  def addValuation(owner: UserId, v: AltValuation): F[Either[PortfolioError, AltValuation]]

  def liabilities(owner: UserId): F[List[Liability]]
  def createLiability(owner: UserId, b: NetWorthService.NewLiability): F[Either[PortfolioError, Liability]]
  def setLiabilityActive(owner: UserId, id: LiabilityId, active: Boolean): F[Option[Liability]]
  def addStatement(owner: UserId, s: LiabilityStatement): F[Either[PortfolioError, LiabilityStatement]]
  def schedule(owner: UserId, id: LiabilityId): F[Either[PortfolioError, NetWorthService.Schedule]]

  def netWorth(owner: UserId, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, NetWorthService.NetWorth]]
  /** Month-end points from `from` to `to`, plus `to` itself. */
  def history(owner: UserId, from: LocalDate, to: LocalDate, base: CurrencyCode): F[Either[PortfolioError, List[NetWorthService.Point]]]
}

object NetWorthService {

  final case class NewAltAsset(
      kind: AltAssetKind,
      name: Title,
      currency: CurrencyCode,
      purchasePrice: BigDecimal,
      purchaseDate: LocalDate,
      ownershipBps: Option[Bps],
      acquisitionCosts: Option[BigDecimal],
      vehicleClass: Option[VehicleClass],
      pace: Option[DepreciationPace],
      notes: Option[Body]
  )

  final case class NewLiability(
      kind: LiabilityKind,
      name: Title,
      currency: CurrencyCode,
      principal: BigDecimal,
      annualRate: BigDecimal,
      termMonths: Int,
      startDate: LocalDate,
      extraMonthlyPayment: Option[BigDecimal],
      securedBy: Option[AltAssetId]
  )

  final case class Schedule(liability: Liability, monthlyPayment: BigDecimal, totalInterest: BigDecimal, payoffDate: Option[LocalDate], rows: List[AmortizationRow])

  final case class Line(id: UUID, name: Title, kind: String, value: BigDecimal)

  final case class NetWorth(asOf: LocalDate, currency: CurrencyCode, accounts: List[Line], assets: List[Line], liabilities: List[Line],
                            totalAssets: BigDecimal, totalLiabilities: BigDecimal, netWorth: BigDecimal)

  final case class Point(date: LocalDate, investments: BigDecimal, otherAssets: BigDecimal, liabilities: BigDecimal, netWorth: BigDecimal)

  private def money(x: BigDecimal) = x.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)

  /**
   * The owner's share of an asset on `date`: the latest valuation, else a
   * vehicle's depreciated price, else the purchase price; nothing before purchase.
   */
  def assetValue(a: AltAsset, valuations: List[AltValuation], date: LocalDate): BigDecimal =
    if (date.isBefore(a.purchaseDate)) BigDecimal(0)
    else {
      val gross = valuations.filter(v => v.assetId == a.id && !v.date.isAfter(date)).maxByOption(_.date).map(_.value)
        .orElse(a.vehicleClass.map(c => Depreciation.value(a.purchasePrice, a.purchaseDate, c, a.pace.getOrElse(DepreciationPace.Average), date)))
        .getOrElse(a.purchasePrice)
      gross * a.ownershipBps.value / 10_000
    }

  def make[F[_]: Async](portfolio: PortfolioService[F], accounts: Accounts[F], market: Market[F], wealth: Wealth[F]): NetWorthService[F] =
    new NetWorthService[F] {

      private type R[A] = EitherT[F, PortfolioError, A]
      private def lift[A](fa: F[A]): R[A] = EitherT.liftF(fa)

      def altAssets(owner: UserId): F[List[AltAsset]] = wealth.assets(owner)

      def createAltAsset(owner: UserId, b: NewAltAsset): F[Either[PortfolioError, AltAsset]] = {
        val checked = for {
          _ <- Either.cond(b.purchasePrice >= 0 && b.acquisitionCosts.forall(_ >= 0), (), PortfolioError.InvalidInput("prices must not be negative"))
          _ <- Either.cond((b.kind == AltAssetKind.Vehicle) == b.vehicleClass.isDefined, (), PortfolioError.InvalidInput("vehicles, and only vehicles, take a vehicle class"))
          _ <- Either.cond(b.ownershipBps.forall(_.value > 0) && b.ownershipBps.forall(_.value <= 10_000), (), PortfolioError.InvalidInput("ownership must be between 1 and 10000 bps"))
        } yield ()
        checked.traverse { _ =>
          for {
            id  <- Async[F].delay(AltAssetId.assume(UUID.randomUUID()))
            now <- Async[F].delay(Instant.now())
            a   <- wealth.createAsset(AltAsset(id, owner, b.kind, b.name, b.currency, b.purchasePrice, b.purchaseDate,
                                               b.ownershipBps.getOrElse(Bps.assume(10_000)), b.acquisitionCosts.getOrElse(BigDecimal(0)),
                                               b.vehicleClass, b.vehicleClass.map(_ => b.pace.getOrElse(DepreciationPace.Average)), b.notes,
                                               active = true, now))
          } yield a
        }
      }

      def setAltAssetActive(owner: UserId, id: AltAssetId, active: Boolean): F[Option[AltAsset]] = wealth.setAssetActive(owner, id, active)

      def addValuation(owner: UserId, v: AltValuation): F[Either[PortfolioError, AltValuation]] =
        wealth.assets(owner).flatMap { as =>
          if (!as.exists(_.id == v.assetId)) Async[F].pure(Left(PortfolioError.AltAssetNotFound(v.assetId)))
          else if (v.value < 0) Async[F].pure(Left(PortfolioError.InvalidInput("a valuation must not be negative")))
          else wealth.upsertValuation(v).as(Right(v))
        }

      def liabilities(owner: UserId): F[List[Liability]] = wealth.liabilities(owner)

      def createLiability(owner: UserId, b: NewLiability): F[Either[PortfolioError, Liability]] =
        wealth.assets(owner).flatMap { as =>
          val checked = for {
            _ <- Either.cond(b.principal > 0, (), PortfolioError.InvalidInput("the principal must be positive"))
            _ <- Either.cond(b.annualRate >= 0 && b.annualRate < 1, (), PortfolioError.InvalidInput("the annual rate is a fraction between 0 and 1"))
            _ <- Either.cond(b.termMonths >= 1 && b.termMonths <= 600, (), PortfolioError.InvalidInput("the term must be 1 to 600 months"))
            _ <- Either.cond(b.extraMonthlyPayment.forall(_ >= 0), (), PortfolioError.InvalidInput("extra payments must not be negative"))
            _ <- b.securedBy.traverse_(id => as.find(_.id == id).toRight(PortfolioError.AltAssetNotFound(id)))
          } yield ()
          checked.traverse { _ =>
            for {
              id  <- Async[F].delay(LiabilityId.assume(UUID.randomUUID()))
              now <- Async[F].delay(Instant.now())
              l   <- wealth.createLiability(Liability(id, owner, b.kind, b.name, b.currency, b.principal, b.annualRate, b.termMonths,
                                                      b.startDate, b.extraMonthlyPayment.getOrElse(BigDecimal(0)), b.securedBy, active = true, now))
            } yield l
          }
        }

      def setLiabilityActive(owner: UserId, id: LiabilityId, active: Boolean): F[Option[Liability]] = wealth.setLiabilityActive(owner, id, active)

      def addStatement(owner: UserId, s: LiabilityStatement): F[Either[PortfolioError, LiabilityStatement]] =
        wealth.liabilities(owner).flatMap { ls =>
          if (!ls.exists(_.id == s.liabilityId)) Async[F].pure(Left(PortfolioError.LiabilityNotFound(s.liabilityId)))
          else if (s.balance < 0) Async[F].pure(Left(PortfolioError.InvalidInput("a balance must not be negative")))
          else wealth.upsertStatement(s).as(Right(s))
        }

      def schedule(owner: UserId, id: LiabilityId): F[Either[PortfolioError, Schedule]] =
        wealth.liabilities(owner).map(_.find(_.id == id).toRight(PortfolioError.LiabilityNotFound(id)).map { l =>
          val rows = Amortization.forLiability(l)
          Schedule(l, Amortization.monthlyPayment(l.principal, l.annualRate, l.termMonths), money(rows.map(_.interest).sum),
                   rows.lastOption.map(_.date), rows)
        })

      private final case class Parts(alt: List[AltAsset], valuations: List[AltValuation], liabilities: List[Liability],
                                     statements: List[LiabilityStatement], fx: FxTable)

      private def parts(owner: UserId): R[Parts] =
        lift((wealth.assets(owner), wealth.valuations(owner), wealth.liabilities(owner), wealth.statements(owner), market.fxRates()).mapN {
          (a, v, l, s, r) => Parts(a.filter(_.active), v, l.filter(_.active), s, FxTable(r))
        })

      private def toBase(p: Parts, amount: BigDecimal, ccy: CurrencyCode, base: CurrencyCode, date: LocalDate): R[BigDecimal] =
        EitherT.fromEither[F](p.fx.convert(amount, ccy, base, date))

      private def otherLines(p: Parts, base: CurrencyCode, date: LocalDate): R[(List[Line], List[Line])] =
        for {
          assets <- p.alt.traverse(a => toBase(p, assetValue(a, p.valuations, date), a.currency, base, date)
                      .map(v => Line(a.id.value, a.name, a.kind.key, money(v))))
          owed   <- p.liabilities.traverse(l => toBase(p, Amortization.balanceOn(l, p.statements.filter(_.liabilityId == l.id), date), l.currency, base, date)
                      .map(v => Line(l.id.value, l.name, l.kind.key, money(v))))
        } yield (assets.filter(_.value != 0), owed.filter(_.value != 0))

      def netWorth(owner: UserId, asOf: LocalDate, base: CurrencyCode): F[Either[PortfolioError, NetWorth]] =
        (for {
          values <- EitherT(portfolio.accountValues(owner, asOf, base))
          accts  <- lift(accounts.list(owner))
          p      <- parts(owner)
          other  <- otherLines(p, base, asOf)
        } yield {
          val acctLines = accts.filter(a => values.get(a.id).exists(_ != 0))
            .map(a => Line(a.id.value, a.name, a.kind.key, money(values(a.id))))
          val assets = acctLines.map(_.value).sum + other._1.map(_.value).sum
          val owed   = other._2.map(_.value).sum
          NetWorth(asOf, base, acctLines, other._1, other._2, money(assets), money(owed), money(assets - owed))
        }).value

      def history(owner: UserId, from: LocalDate, to: LocalDate, base: CurrencyCode): F[Either[PortfolioError, List[Point]]] =
        (for {
          _      <- EitherT.cond[F](!from.isAfter(to), (), PortfolioError.InvalidPeriod(from, to): PortfolioError)
          series <- EitherT(portfolio.history(owner, None, from, to, base))
          p      <- parts(owner)
          dates   = (Iterator.iterate(YearMonth.from(from))(_.plusMonths(1)).takeWhile(!_.isAfter(YearMonth.from(to)))
                      .map(_.atEndOfMonth).filter(d => !d.isAfter(to)).toList :+ to).distinct
          byDate  = series.map(d => d.date -> d.total).toMap
          points <- dates.traverse { d =>
                      otherLines(p, base, d).map { case (assets, owed) =>
                        val inv = byDate.getOrElse(d, BigDecimal(0))
                        val oth = assets.map(_.value).sum
                        val lia = owed.map(_.value).sum
                        Point(d, money(inv), money(oth), money(lia), money(inv + oth - lia))
                      }
                    }
        } yield points).value
    }
}
