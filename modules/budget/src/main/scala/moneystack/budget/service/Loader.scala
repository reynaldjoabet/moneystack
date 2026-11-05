package moneystack.budget.service

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.budget.db.{Catalog, Transactions, Wallets}
import moneystack.platform.FxTable

import cats.effect.*
import cats.syntax.all.*

import java.time.LocalDate

/** What the reading services need: the wallets a user can see, their transactions, categories and rates. */
final case class Snapshot(wallets: List[Wallet], transactions: List[Transaction], categories: List[Category], fx: FxTable) {
  val walletsById: Map[WalletId, Wallet] = wallets.map(w => w.id -> w).toMap

  /** Income and spending entries in `currency`, optionally limited to some wallets. */
  def entries(currency: CurrencyCode, only: Option[List[WalletId]] = None): Either[BudgetError, List[Entry]] =
    Reports.entries(transactions.filter(t => only.forall(_.contains(t.walletId))), walletsById, fx, currency)
}

final class Loader[F[_]: Async](wallets: Wallets[F], catalog: Catalog[F], transactions: Transactions[F]) {

  /** The user's own wallets plus those shared with them. */
  def walletsOf(user: UserId): F[List[Wallet]] = (wallets.owned(user), wallets.sharedWith(user)).mapN(_ ++ _)

  private def snapshot(user: UserId, ws: List[Wallet], txs: List[Transaction]): F[Snapshot] =
    (ws.map(_.ownerId).appended(user).distinct.flatTraverse(catalog.categories), catalog.rates())
      .mapN((cats, rates) => Snapshot(ws, txs, cats, FxTable(rates)))

  /** Everything: every transaction the user can see, with their owners' categories. Prefer a narrower load. */
  def load(user: UserId, includeArchived: Boolean = true): F[Snapshot] =
    walletsOf(user).map(_.filter(w => includeArchived || !w.archived)).flatMap { ws =>
      transactions.forWallets(ws.map(_.id)).flatMap(snapshot(user, ws, _))
    }

  def moved(ws: List[Wallet], asOf: LocalDate): F[Map[WalletId, Long]] = transactions.moved(ws.map(_.id), asOf)

  /** The days `user` recorded anything in a wallet they can see. */
  def loggedDays(user: UserId): F[Set[LocalDate]] = walletsOf(user).flatMap(ws => transactions.loggedDays(ws.map(_.id), user))

  /** As [[load]], without transactions. */
  def reference(user: UserId): F[Snapshot] = walletsOf(user).flatMap(snapshot(user, _, Nil))

  /**
   * As [[load]], for reports that only look inside `period`. A refund takes its category from the transaction it
   * refunds, which may be older than the period, so those come too. They sit outside the period, so reports skip them.
   */
  def within(user: UserId, period: Period): F[Snapshot] =
    walletsOf(user).flatMap { ws =>
      val ids = ws.map(_.id).toSet
      for {
        txs <- transactions.between(ws.map(_.id), period.from, period.to)
        known = txs.map(_.id).toSet
        originals <- transactions.byIds(txs.flatMap(_.refundOf).distinct.filterNot(known))
        visible = originals.filter(t => ids(t.walletId) || t.toWalletId.exists(ids))
        snap <- snapshot(user, ws, visible ++ txs)
      } yield snap
    }
}
