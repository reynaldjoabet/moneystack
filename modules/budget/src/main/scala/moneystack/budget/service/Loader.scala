package moneystack.budget.service

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.budget.db.{Catalog, Transactions, Wallets}
import moneystack.platform.FxTable

import cats.effect.*
import cats.syntax.all.*

/** What the reading services need: the wallets a user can see, their transactions, categories and rates. */
final case class Snapshot(wallets: List[Wallet], transactions: List[Transaction], categories: List[Category], fx: FxTable) {
  val walletsById: Map[WalletId, Wallet] = wallets.map(w => w.id -> w).toMap

  /** Income and spending entries in `currency`, optionally limited to some wallets. */
  def entries(currency: CurrencyCode, only: Option[List[WalletId]] = None): Either[BudgetError, List[Entry]] =
    Reports.entries(transactions.filter(t => only.forall(_.contains(t.walletId))), walletsById, fx, currency)
}

final class Loader[F[_]: Async](wallets: Wallets[F], catalog: Catalog[F], transactions: Transactions[F]) {

  /** The user's own wallets plus those shared with them (with their owners' categories). */
  def load(user: UserId, includeArchived: Boolean = true): F[Snapshot] =
    for {
      own    <- wallets.owned(user)
      shared <- wallets.sharedWith(user)
      ws = (own ++ shared).filter(w => includeArchived || !w.archived)
      txs   <- transactions.forWallets(ws.map(_.id))
      cats  <- ws.map(_.ownerId).appended(user).distinct.flatTraverse(catalog.categories)
      rates <- catalog.rates()
    } yield Snapshot(ws, txs, cats, FxTable(rates))
}
