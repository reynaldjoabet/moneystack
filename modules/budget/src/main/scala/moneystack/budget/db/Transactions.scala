package moneystack.budget.db

import moneystack.domain.UserId
import moneystack.budget.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.{Command, Query, Session}
import skunk.implicits.*

import java.time.LocalDate

/** Transactions with their splits. Writes that must commit with other changes take a session. */
trait Transactions[F[_]] {
  def insert(t: Transaction): F[Transaction]

  /** Inserts unless the schedule occurrence or import key is already recorded; `None` if it was. */
  def insertIfNewIn(s: Session[F], t: Transaction): F[Option[Transaction]]
  def replace(t: Transaction): F[Option[Transaction]]
  def delete(id: TransactionId, wallets: List[WalletId]): F[Boolean]

  /** Transactions touching any of `wallets` (as source or transfer destination). */
  def forWallets(wallets: List[WalletId]): F[List[Transaction]]

  /** As [[forWallets]], dated `from` to `to` inclusive. */
  def between(wallets: List[WalletId], from: LocalDate, to: LocalDate): F[List[Transaction]]

  def find(id: TransactionId): F[Option[Transaction]]

  /** These transactions, without their splits. */
  def byIds(ids: List[TransactionId]): F[List[Transaction]]

  /** What transactions dated up to `asOf` moved in or out of each of `wallets`. */
  def moved(wallets: List[WalletId], asOf: LocalDate): F[Map[WalletId, Long]]

  /** As [[moved]], per day, for every day up to `asOf` on which something moved. */
  def movedByDay(wallets: List[WalletId], asOf: LocalDate): F[List[(WalletId, LocalDate, Long)]]

  /** The days on which `user` recorded a transaction touching `wallets`. */
  def loggedDays(wallets: List[WalletId], user: UserId): F[Set[LocalDate]]

  /** The import keys already recorded in `wallet`, for de-duplicating a CSV import. */
  def importKeys(wallet: WalletId): F[Set[String]]
}

object Transactions {

  import Codecs.{split as splitC, transaction as txC, transactionId as txIdC, userId as userIdC, walletId as walletIdC}
  import skunk.codec.all.{date, int4, int8, varchar}

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
      pool.use(s =>
        s.transaction.use { _ =>
          s.prepare(Q.replace).flatMap(_.option(t)).flatMap(_.traverse(_ => writeSplits(s, t).as(t)))
        }
      )

    def delete(id: TransactionId, wallets: List[WalletId]): F[Boolean] =
      if (wallets.isEmpty) Concurrent[F].pure(false)
      else pool.use(_.prepare(Q.delete(wallets.size)).flatMap(_.execute((id, wallets)))).map {
        case skunk.data.Completion.Delete(n) => n > 0
        case _                               => false
      }

    /** Attaches each transaction's splits. */
    private def withSplits(txs: List[Transaction], splits: List[(TransactionId, Split)]): List[Transaction] = {
      val byTx = splits.groupMap(_._1)(_._2)
      txs.map(t => t.copy(splits = byTx.getOrElse(t.id, Nil)))
    }

    def forWallets(wallets: List[WalletId]): F[List[Transaction]] =
      if (wallets.isEmpty) Concurrent[F].pure(Nil)
      else pool.use { s =>
        val args = (wallets, wallets)
        (
          s.prepare(Q.forWallets(wallets.size)).flatMap(_.stream(args, 512).compile.toList),
          s.prepare(Q.splitsForWallets(wallets.size)).flatMap(_.stream(args, 512).compile.toList)
        ).mapN(withSplits)
      }

    def between(wallets: List[WalletId], from: LocalDate, to: LocalDate): F[List[Transaction]] =
      if (wallets.isEmpty) Concurrent[F].pure(Nil)
      else pool.use { s =>
        val args = (wallets, wallets, from, to)
        (
          s.prepare(Q.between(wallets.size)).flatMap(_.stream(args, 512).compile.toList),
          s.prepare(Q.splitsBetween(wallets.size)).flatMap(_.stream(args, 512).compile.toList)
        ).mapN(withSplits)
      }

    def find(id: TransactionId): F[Option[Transaction]] =
      pool.use { s =>
        s.prepare(Q.byId).flatMap(_.option(id)).flatMap(_.traverse(t =>
          s.prepare(Q.splitsOf).flatMap(_.stream(id, 64).compile.toList).map(sp => t.copy(splits = sp))
        ))
      }

    def byIds(ids: List[TransactionId]): F[List[Transaction]] =
      if (ids.isEmpty) Concurrent[F].pure(Nil)
      else pool.use(_.prepare(Q.byIds(ids.size)).flatMap(_.stream(ids, 256).compile.toList))

    def moved(wallets: List[WalletId], asOf: LocalDate): F[Map[WalletId, Long]] =
      if (wallets.isEmpty) Concurrent[F].pure(Map.empty)
      else pool.use(_.prepare(Q.moved(wallets.size)).flatMap(_.stream((wallets, asOf, wallets, asOf), 256).compile.to(Map)))

    def movedByDay(wallets: List[WalletId], asOf: LocalDate): F[List[(WalletId, LocalDate, Long)]] =
      if (wallets.isEmpty) Concurrent[F].pure(Nil)
      else pool.use(_.prepare(Q.movedByDay(wallets.size)).flatMap(_.stream((wallets, asOf, wallets, asOf), 1024).compile.toList))

    def loggedDays(wallets: List[WalletId], user: UserId): F[Set[LocalDate]] =
      if (wallets.isEmpty) Concurrent[F].pure(Set.empty)
      else pool.use(_.prepare(Q.loggedDays(wallets.size)).flatMap(_.stream((user, (wallets, wallets)), 1024).compile.to(Set)))

    def importKeys(wallet: WalletId): F[Set[String]] =
      pool.use(_.prepare(Q.importKeys).flatMap(_.stream(wallet, 1024).compile.to(Set)))
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

    /** Transactions touching one of `n` wallets. Splits are matched by joining on this too, not by listing transaction ids. */
    private def touching(n: Int) =
      sql"(t.wallet_id IN (${walletIdC.list(n)}) OR t.to_wallet_id IN (${walletIdC.list(n)}))"

    def forWallets(n: Int): Query[(List[WalletId], List[WalletId]), Transaction] =
      sql"SELECT $cols FROM budget.transactions t WHERE ${touching(n)} ORDER BY tx_date, created_at".query(txC)

    def between(n: Int): Query[(List[WalletId], List[WalletId], LocalDate, LocalDate), Transaction] =
      sql"""SELECT $cols FROM budget.transactions t WHERE ${touching(n)} AND t.tx_date BETWEEN $date AND $date
            ORDER BY tx_date, created_at""".query(txC)
        .contramap[(List[WalletId], List[WalletId], LocalDate, LocalDate)] { case (a, b, f, to) => ((a, b), f, to) }

    val byId: Query[TransactionId, Transaction] =
      sql"SELECT $cols FROM budget.transactions WHERE id = $txIdC".query(txC)

    def byIds(n: Int): Query[List[TransactionId], Transaction] =
      sql"SELECT $cols FROM budget.transactions WHERE id IN (${txIdC.list(n)})".query(txC)

    /** The SQL form of `Reports.effects`: the two sides of a transfer land in different wallets. */
    def moved(n: Int): Query[(List[WalletId], LocalDate, List[WalletId], LocalDate), (WalletId, Long)] =
      sql"""SELECT wallet, sum(delta)::int8 FROM (
              SELECT wallet_id AS wallet, CASE kind WHEN 'income' THEN amount_minor ELSE -amount_minor END AS delta
              FROM budget.transactions WHERE wallet_id IN (${walletIdC.list(n)}) AND tx_date <= $date
              UNION ALL
              SELECT to_wallet_id, coalesce(to_amount_minor, amount_minor)
              FROM budget.transactions WHERE to_wallet_id IN (${walletIdC.list(n)}) AND tx_date <= $date
            ) AS movements GROUP BY wallet""".query(walletIdC *: int8)

    def movedByDay(n: Int): Query[(List[WalletId], LocalDate, List[WalletId], LocalDate), (WalletId, LocalDate, Long)] =
      sql"""SELECT wallet, day, sum(delta)::int8 FROM (
              SELECT wallet_id AS wallet, tx_date AS day, CASE kind WHEN 'income' THEN amount_minor ELSE -amount_minor END AS delta
              FROM budget.transactions WHERE wallet_id IN (${walletIdC.list(n)}) AND tx_date <= $date
              UNION ALL
              SELECT to_wallet_id, tx_date, coalesce(to_amount_minor, amount_minor)
              FROM budget.transactions WHERE to_wallet_id IN (${walletIdC.list(n)}) AND tx_date <= $date
            ) AS movements GROUP BY wallet, day""".query(walletIdC *: date *: int8)

    def loggedDays(n: Int): Query[(UserId, (List[WalletId], List[WalletId])), LocalDate] =
      sql"SELECT DISTINCT t.tx_date FROM budget.transactions t WHERE t.created_by = $userIdC AND ${touching(n)}".query(date)

    val importKeys: Query[WalletId, String] =
      sql"SELECT import_key FROM budget.transactions WHERE wallet_id = $walletIdC AND import_key IS NOT NULL".query(varchar(64))

    val clearSplits: Command[TransactionId] =
      sql"DELETE FROM budget.transaction_splits WHERE transaction_id = $txIdC".command

    val insertSplit: Command[(TransactionId, Int, Split)] =
      sql"""INSERT INTO budget.transaction_splits (transaction_id, position, category_id, amount_minor, note)
            VALUES ($txIdC, $int4, $splitC)""".command

    private val splitCols = sql"s.transaction_id, s.category_id, s.amount_minor, s.note"

    def splitsForWallets(n: Int): Query[(List[WalletId], List[WalletId]), (TransactionId, Split)] =
      sql"""SELECT $splitCols FROM budget.transaction_splits s JOIN budget.transactions t ON t.id = s.transaction_id
            WHERE ${touching(n)} ORDER BY s.transaction_id, s.position""".query(txIdC *: splitC)

    def splitsBetween(n: Int): Query[(List[WalletId], List[WalletId], LocalDate, LocalDate), (TransactionId, Split)] =
      sql"""SELECT $splitCols FROM budget.transaction_splits s JOIN budget.transactions t ON t.id = s.transaction_id
            WHERE ${touching(n)} AND t.tx_date BETWEEN $date AND $date ORDER BY s.transaction_id, s.position""".query(txIdC *: splitC)
        .contramap[(List[WalletId], List[WalletId], LocalDate, LocalDate)] { case (a, b, f, to) => ((a, b), f, to) }

    val splitsOf: Query[TransactionId, Split] =
      sql"""SELECT category_id, amount_minor, note FROM budget.transaction_splits
            WHERE transaction_id = $txIdC ORDER BY position""".query(splitC)
  }
}
