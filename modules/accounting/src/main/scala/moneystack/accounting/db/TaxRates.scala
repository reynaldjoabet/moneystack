package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait TaxRates[F[_]] {
  def create(t: TaxRate): F[TaxRate]
  def list(): F[List[TaxRate]]
  def setActive(id: TaxRateId, active: Boolean): F[Option[TaxRate]]
}

object TaxRates {

  import Codecs.{taxRate as taxRateC, taxRateId as taxRateIdC}
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): TaxRates[F] =
    new TaxRates[F] {

      def create(t: TaxRate): F[TaxRate] =
        pool.use(_.prepare(Q.insert).flatMap(_.unique(t)))

      def list(): F[List[TaxRate]] =
        pool.use(_.prepare(Q.list).flatMap(_.stream(skunk.Void, 64).compile.toList))

      def setActive(id: TaxRateId, active: Boolean): F[Option[TaxRate]] =
        pool.use(_.prepare(Q.setActive).flatMap(_.option((active, id))))
    }

  private object Q {

    private val cols = sql"id, code, name, rate_bps, active, created_at"

    val insert: Query[TaxRate, TaxRate] =
      sql"INSERT INTO tax_rates ($cols) VALUES ${taxRateC.values} RETURNING $cols".query(taxRateC)

    val list: Query[skunk.Void, TaxRate] =
      sql"SELECT $cols FROM tax_rates ORDER BY code".query(taxRateC)

    val setActive: Query[(Boolean, TaxRateId), TaxRate] =
      sql"UPDATE tax_rates SET active = $bool WHERE id = $taxRateIdC RETURNING $cols".query(taxRateC)
  }
}
