package moneystack.budget.db

import moneystack.domain.*
import moneystack.budget.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.{Command, Query, Session}
import skunk.implicits.*

/** Wallets and their read-only shares. */
trait Wallets[F[_]] {
  def create(w: Wallet): F[Wallet]
  def owned(owner: UserId): F[List[Wallet]]
  /** Wallets other users shared with `user`. */
  def sharedWith(user: UserId): F[List[Wallet]]
  def setArchived(owner: UserId, id: WalletId, archived: Boolean): F[Option[Wallet]]
  def share(s: WalletShare): F[Unit]
  def unshare(walletId: WalletId, user: UserId): F[Boolean]
  def shares(owner: UserId): F[List[WalletShare]]
}

object Wallets {

  import Codecs.{share as shareC, userId as userIdC, wallet as walletC, walletId as walletIdC}
  import skunk.codec.all.bool

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Wallets[F] = new Wallets[F] {

    def create(w: Wallet): F[Wallet] = pool.use(_.prepare(Q.insert).flatMap(_.unique(w)))

    def owned(owner: UserId): F[List[Wallet]] = pool.use(_.prepare(Q.owned).flatMap(_.stream(owner, 64).compile.toList))

    def sharedWith(user: UserId): F[List[Wallet]] = pool.use(_.prepare(Q.sharedWith).flatMap(_.stream(user, 64).compile.toList))

    def setArchived(owner: UserId, id: WalletId, archived: Boolean): F[Option[Wallet]] =
      pool.use(_.prepare(Q.setArchived).flatMap(_.option((archived, id, owner))))

    def share(s: WalletShare): F[Unit] = pool.use(_.prepare(Q.share).flatMap(_.execute(s))).void

    def unshare(walletId: WalletId, user: UserId): F[Boolean] =
      pool.use(_.prepare(Q.unshare).flatMap(_.execute((walletId, user)))).map {
        case skunk.data.Completion.Delete(n) => n > 0
        case _                               => false
      }

    def shares(owner: UserId): F[List[WalletShare]] = pool.use(_.prepare(Q.shares).flatMap(_.stream(owner, 64).compile.toList))
  }

  private object Q {

    private val cols  = sql"id, owner_id, name, kind, currency, opening_balance_minor, archived, created_at"
    private val wCols = sql"w.id, w.owner_id, w.name, w.kind, w.currency, w.opening_balance_minor, w.archived, w.created_at"

    val insert: Query[Wallet, Wallet] =
      sql"INSERT INTO budget.wallets ($cols) VALUES ${walletC.values} RETURNING $cols".query(walletC)

    val owned: Query[UserId, Wallet] =
      sql"SELECT $cols FROM budget.wallets WHERE owner_id = $userIdC ORDER BY archived, name".query(walletC)

    val sharedWith: Query[UserId, Wallet] =
      sql"""SELECT $wCols FROM budget.wallets w JOIN budget.wallet_shares s ON s.wallet_id = w.id
            WHERE s.user_id = $userIdC ORDER BY w.name""".query(walletC)

    val setArchived: Query[(Boolean, WalletId, UserId), Wallet] =
      sql"UPDATE budget.wallets SET archived = $bool WHERE id = $walletIdC AND owner_id = $userIdC RETURNING $cols".query(walletC)

    val share: Command[WalletShare] =
      sql"""INSERT INTO budget.wallet_shares (wallet_id, user_id, created_at) VALUES ${shareC.values}
            ON CONFLICT (wallet_id, user_id) DO NOTHING""".command

    val unshare: Command[(WalletId, UserId)] =
      sql"DELETE FROM budget.wallet_shares WHERE wallet_id = $walletIdC AND user_id = $userIdC".command

    val shares: Query[UserId, WalletShare] =
      sql"""SELECT s.wallet_id, s.user_id, s.created_at FROM budget.wallet_shares s
            JOIN budget.wallets w ON w.id = s.wallet_id WHERE w.owner_id = $userIdC""".query(shareC)
  }
}
