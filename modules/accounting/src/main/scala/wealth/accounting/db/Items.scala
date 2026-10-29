package wealth.accounting.db

import wealth.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

import java.time.LocalDate
import java.util.UUID

/** Item catalogue plus the inventory sub-ledger (stock positions, moves, adjustments). */
trait Items[F[_]] {
  /** Inserts the item and, for inventory items, its zero stock position. */
  def create(i: Item): F[Item]
  def list(): F[List[Item]]
  def setActive(id: ItemId, active: Boolean): F[Option[Item]]

  /** Locks the positions in id order (so concurrent postings can't deadlock) and returns them. */
  def lockStock(s: Session[F], ids: List[ItemId]): F[Map[ItemId, Stock]]
  /** Records `move` and sets the position to `after`. */
  def move(s: Session[F], m: InventoryMove, after: Stock): F[Unit]
  def movesFor(s: Session[F], source: JournalSource, sourceId: UUID): F[List[InventoryMove]]

  def insertAdjustment(s: Session[F], a: InventoryAdjustment): F[Unit]
  def adjustments(limit: Int): F[List[InventoryAdjustment]]

  /** Quantity and value per item from moves dated on or before `asOf`. */
  def valuation(asOf: LocalDate): F[List[Stock]]
}

object Items {

  import Codecs.{inventoryAdjustment as adjustmentC, inventoryMove as moveC, item as itemC, itemId as itemIdC,
                 journalSource as sourceC, stock as stockC}
  import skunk.codec.all.{bool, date, int4, uuid}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Items[F] =
    new Items[F] {

      def create(i: Item): F[Item] =
        pool.use { s =>
          s.transaction.use { _ =>
            for {
              saved <- s.prepare(Q.insert).flatMap(_.unique(i))
              _     <- s.prepare(Q.insertStock).flatMap(_.execute(i.id)).whenA(i.kind == ItemKind.Inventory)
            } yield saved
          }
        }

      def list(): F[List[Item]] =
        pool.use(_.prepare(Q.list).flatMap(_.stream(skunk.Void, 128).compile.toList))

      def setActive(id: ItemId, active: Boolean): F[Option[Item]] =
        pool.use(_.prepare(Q.setActive).flatMap(_.option((active, id))))

      def lockStock(s: Session[F], ids: List[ItemId]): F[Map[ItemId, Stock]] =
        s.prepare(Q.stockForUpdate).flatMap { pq =>
          ids.distinct.sortBy(_.value).traverse(id => pq.unique(id).map(id -> _))
        }.map(_.toMap)

      def move(s: Session[F], m: InventoryMove, after: Stock): F[Unit] =
        for {
          _ <- s.prepare(Q.insertMove).flatMap(_.execute(m))
          _ <- s.prepare(Q.setStock).flatMap(_.execute(after))
        } yield ()

      def movesFor(s: Session[F], source: JournalSource, sourceId: UUID): F[List[InventoryMove]] =
        s.prepare(Q.movesFor).flatMap(_.stream((source, sourceId), 64).compile.toList)

      def insertAdjustment(s: Session[F], a: InventoryAdjustment): F[Unit] =
        s.prepare(Q.insertAdjustment).flatMap(_.execute(a)).void

      def adjustments(limit: Int): F[List[InventoryAdjustment]] =
        pool.use(_.prepare(Q.adjustments).flatMap(_.stream(limit, 64).compile.toList))

      def valuation(asOf: LocalDate): F[List[Stock]] =
        pool.use(_.prepare(Q.valuation).flatMap(_.stream(asOf, 128).compile.toList))
    }

  private object Q {

    private val cols =
      sql"""id, sku, name, kind, sell_price_minor, cost_price_minor,
            income_account_id, expense_account_id, inventory_account_id, active, created_at"""

    val insert: Query[Item, Item] =
      sql"INSERT INTO items ($cols) VALUES ${itemC.values} RETURNING $cols".query(itemC)

    val insertStock: Command[ItemId] =
      sql"INSERT INTO item_stock (item_id) VALUES ($itemIdC)".command

    val list: Query[skunk.Void, Item] =
      sql"SELECT $cols FROM items ORDER BY sku".query(itemC)

    val setActive: Query[(Boolean, ItemId), Item] =
      sql"UPDATE items SET active = $bool WHERE id = $itemIdC RETURNING $cols".query(itemC)

    val stockForUpdate: Query[ItemId, Stock] =
      sql"SELECT item_id, quantity, value_minor FROM item_stock WHERE item_id = $itemIdC FOR UPDATE".query(stockC)

    val setStock: Command[Stock] =
      sql"UPDATE item_stock SET quantity = ${Codecs.decimal}, value_minor = ${skunk.codec.all.int8} WHERE item_id = $itemIdC"
        .command.contramap[Stock](st => (st.quantity, st.valueMinor, st.itemId))

    private val moveCols = sql"item_id, move_date, quantity, value_minor, source, source_id"

    val insertMove: Command[InventoryMove] =
      sql"INSERT INTO inventory_moves ($moveCols) VALUES ${moveC.values}".command

    val movesFor: Query[(JournalSource, UUID), InventoryMove] =
      sql"SELECT $moveCols FROM inventory_moves WHERE source = $sourceC AND source_id = $uuid ORDER BY id".query(moveC)

    private val adjustmentCols =
      sql"id, item_id, adjust_date, quantity, value_minor, account_id, reason, entry_id, created_by, created_at"

    val insertAdjustment: Command[InventoryAdjustment] =
      sql"INSERT INTO inventory_adjustments ($adjustmentCols) VALUES ${adjustmentC.values}".command

    val adjustments: Query[Int, InventoryAdjustment] =
      sql"SELECT $adjustmentCols FROM inventory_adjustments ORDER BY adjust_date DESC, created_at DESC LIMIT $int4"
        .query(adjustmentC)

    val valuation: Query[LocalDate, Stock] =
      sql"""SELECT item_id, SUM(quantity)::numeric(20, 6), SUM(value_minor)::int8
            FROM inventory_moves WHERE move_date <= $date
            GROUP BY item_id""".query(stockC)
  }
}
