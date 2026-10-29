package moneystack.budget.db

import moneystack.budget.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.{Command, Query, Session}
import skunk.implicits.*

/** Transactions with their splits. Writes that must commit with other changes take a session. */
trait Transactions[F[_]] {
  def insert(t: Transaction): F[Transaction]
  /** Inserts unless the schedule occurrence or import key is already recorded; `None` if it was. */
  def insertIfNewIn(s: Session[F], t: Transaction): F[Option[Transaction]]
  def replace(t: Transaction): F[Option[Transaction]]
  def delete(id: TransactionId, wallets: List[WalletId]): F[Boolean]
  /** Transactions touching any of `wallets` (as source or transfer destination). */
  def forWallets(wallets: List[WalletId]): F[List[Transaction]]
}

object Transactions {

  import Codecs.{split as splitC, transaction as txC, transactionId as txIdC, walletId as walletIdC}
  import skunk.codec.all.int4

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Transactions[F] = new Transactions[F] {

    private def writeSplits(s: Session[F], t: Transaction): F[Unit] =
      s.prepare(Q.clearSplits).flatMap(_.execute(t.id)) *>
        s.prepare(Q.insertSplit).flatMap(pc => t.splits.zipWithIndex.traverse_ { case (sp, i) => pc.execute((t.id, i + 1, sp)) })

    def insert(t: Transaction): F[Transaction] =
      pool.use(s => s.transaction.use(_ => s.prepare(Q.insert).flatMap(_.execute(t)) *> writeSplits(s, t).as(t)))

    def insertIfNewIn(s: Session[F], t: Transaction): F[Option[Transaction]] =
      s.prepare(Q.insertIfNew).flatMap(_.option(t)).flatMap {
        case Some(_) => writeSplits(s, t).as(Some(t))
        case None    => Concurrent[F].pure(None)
      }

    def replace(t: Transaction): F[Option[Transaction]] =
      pool.use(s => s.transaction.use { _ =>
        s.prepare(Q.replace).flatMap(_.option(t)).flatMap(_.traverse(_ => writeSplits(s, t).as(t)))
      })

    def delete(id: TransactionId, wallets: List[WalletId]): F[Boolean] =
      if (wallets.isEmpty) Concurrent[F].pure(false)
      else pool.use(_.prepare(Q.delete(wallets.size)).flatMap(_.execute((id, wallets)))).map {
        case skunk.data.Completion.Delete(n) => n > 0
        case _                               => false
      }

    def forWallets(wallets: List[WalletId]): F[List[Transaction]] =
      if (wallets.isEmpty) Concurrent[F].pure(Nil)
      else pool.use { s =>
        for {
          txs    <- s.prepare(Q.forWallets(wallets.size)).flatMap(_.stream((wallets, wallets), 512).compile.toList)
          splits <- if (txs.isEmpty) Concurrent[F].pure(Nil)
                    else s.prepare(Q.splitsFor(txs.size)).flatMap(_.stream(txs.map(_.id), 512).compile.toList)
          byTx    = splits.groupMap(_._1)(_._2)
        } yield txs.map(t => t.copy(splits = byTx.getOrElse(t.id, Nil)))
      }
  }

  private object Q {

    private val cols =
      sql"""id, owner_id, wallet_id, kind, amount_minor, tx_date, title, note, category_id, category_source, payee_id, tags,
            to_wallet_id, to_amount_minor, refund_of, objective_id, schedule_id, due_date, excluded_budgets, import_key,
            created_by, created_at"""

    val insert: Command[Transaction] = sql"INSERT INTO budget.transactions ($cols) VALUES ${txC.values}".command

    val insertIfNew: Query[Transaction, TransactionId] =
      sql"INSERT INTO budget.transactions ($cols) VALUES ${txC.values} ON CONFLICT DO NOTHING RETURNING id".query(txIdC)

    val replace: Query[Transaction, TransactionId] =
      sql"UPDATE budget.transactions SET ($cols) = ($txC) WHERE id = $txIdC RETURNING id".query(txIdC)
        .contramap[Transaction](t => (t, t.id))

    def delete(n: Int): Command[(TransactionId, List[WalletId])] =
      sql"DELETE FROM budget.transactions WHERE id = $txIdC AND wallet_id IN (${walletIdC.list(n)})".command

    def forWallets(n: Int): Query[(List[WalletId], List[WalletId]), Transaction] =
      sql"""SELECT $cols FROM budget.transactions
            WHERE wallet_id IN (${walletIdC.list(n)}) OR to_wallet_id IN (${walletIdC.list(n)})
            ORDER BY tx_date, created_at""".query(txC)

    val clearSplits: Command[TransactionId] =
      sql"DELETE FROM budget.transaction_splits WHERE transaction_id = $txIdC".command

    val insertSplit: Command[(TransactionId, Int, Split)] =
      sql"""INSERT INTO budget.transaction_splits (transaction_id, position, category_id, amount_minor, note)
            VALUES ($txIdC, $int4, $splitC)""".command

    def splitsFor(n: Int): Query[List[TransactionId], (TransactionId, Split)] =
      sql"""SELECT transaction_id, category_id, amount_minor, note FROM budget.transaction_splits
            WHERE transaction_id IN (${txIdC.list(n)}) ORDER BY transaction_id, position""".query(txIdC *: splitC)
  }
}
