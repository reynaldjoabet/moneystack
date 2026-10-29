package moneystack.accounting.db

import moneystack.domain.*
import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

import java.time.LocalDate

/** Daily rates into the ledger's base currency. */
trait ExchangeRates[F[_]] {
  def upsert(r: FxRate): F[Unit]
  /** The most recent rate published on or before `date`. */
  def rateOn(currency: CurrencyCode, date: LocalDate): F[Option[ExchangeRate]]
  def history(currency: CurrencyCode, limit: Int): F[List[FxRate]]
}

object ExchangeRates {

  import Codecs.{currency as currencyC, exchangeRate as rateC, fxRate as fxRateC}
  import skunk.codec.all.{date, int4}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): ExchangeRates[F] =
    new ExchangeRates[F] {

      def upsert(r: FxRate): F[Unit] =
        pool.use(_.prepare(Q.upsert).flatMap(_.execute(r))).void

      def rateOn(currency: CurrencyCode, d: LocalDate): F[Option[ExchangeRate]] =
        pool.use(_.prepare(Q.rateOn).flatMap(_.option((currency, d))))

      def history(currency: CurrencyCode, limit: Int): F[List[FxRate]] =
        pool.use(_.prepare(Q.history).flatMap(_.stream((currency, limit), 64).compile.toList))
    }

  private object Q {

    val upsert: Command[FxRate] =
      sql"""INSERT INTO exchange_rates (currency, rate_date, rate)
            VALUES ${fxRateC.values}
            ON CONFLICT (currency, rate_date) DO UPDATE SET rate = EXCLUDED.rate""".command

    val rateOn: Query[(CurrencyCode, LocalDate), ExchangeRate] =
      sql"""SELECT rate FROM exchange_rates
            WHERE currency = $currencyC AND rate_date <= $date
            ORDER BY rate_date DESC LIMIT 1""".query(rateC)

    val history: Query[(CurrencyCode, Int), FxRate] =
      sql"""SELECT currency, rate_date, rate FROM exchange_rates
            WHERE currency = $currencyC
            ORDER BY rate_date DESC LIMIT $int4""".query(fxRateC)
  }
}
