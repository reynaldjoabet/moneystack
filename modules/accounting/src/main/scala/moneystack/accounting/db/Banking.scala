package moneystack.accounting.db

import moneystack.accounting.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

/** Bank feed transactions and the rules that categorise them. */
trait Banking[F[_]] {
  def createRule(r: BankRule): F[BankRule]
  def rules(): F[List[BankRule]]
  def setRuleActive(id: BankRuleId, active: Boolean): F[Boolean]

  /** Inserts each transaction unless `(accountId, externalId)` was already imported; returns the new ones. */
  def insertTransactions(txs: List[BankTransaction]): F[List[BankTransaction]]
  def transactions(account: Option[AccountId], status: Option[BankTxStatus], limit: Int): F[List[BankTransaction]]
  def findForUpdate(s: Session[F], id: BankTransactionId): F[Option[BankTransaction]]
  def update(s: Session[F], tx: BankTransaction): F[Unit]
  def setRule(id: BankTransactionId, rule: Option[BankRuleId]): F[Unit]
}

object Banking {

  import Codecs.{
    accountId as accountIdC, bankRule as ruleC, bankRuleId as ruleIdC, bankTransaction as txC,
    bankTransactionId as txIdC, bankTxStatus as statusC, contactId as contactIdC,
    journalEntryId as entryIdC, ruleCondition as conditionC
  }
  import skunk.codec.all.{bool, int4, uuid}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Banking[F] =
    new Banking[F] {

      def createRule(r: BankRule): F[BankRule] =
        pool.use { s =>
          s.transaction.use { _ =>
            for {
              _ <- s.prepare(Q.insertRule).flatMap(_.execute(r))
              _ <- s.prepare(Q.insertCondition).flatMap(pc =>
                r.conditions.zipWithIndex.traverse_ { case (c, i) => pc.execute((r.id, i + 1, c)) }
              )
            } yield r
          }
        }

      def rules(): F[List[BankRule]] =
        pool.use { s =>
          for {
            rs <- s.prepare(Q.rules).flatMap(_.stream(skunk.Void, 64).compile.toList)
            cs <- s.prepare(Q.conditions).flatMap(_.stream(skunk.Void, 256).compile.toList)
            byRule = cs.groupMap(_._1)(_._2)
          } yield rs.map(r => r.copy(conditions = byRule.getOrElse(r.id, Nil)))
        }

      def setRuleActive(id: BankRuleId, active: Boolean): F[Boolean] =
        pool.use(_.prepare(Q.setRuleActive).flatMap(_.execute((active, id)))).map {
          case skunk.data.Completion.Update(n) => n > 0
          case _                               => false
        }

      def insertTransactions(txs: List[BankTransaction]): F[List[BankTransaction]] =
        pool.use { s =>
          s.transaction.use { _ =>
            s.prepare(Q.insertTx).flatMap(pq => txs.flatTraverse(tx => pq.option(tx).map(_.toList)))
          }
        }

      def transactions(account: Option[AccountId], status: Option[BankTxStatus], limit: Int): F[List[BankTransaction]] =
        pool.use(_.prepare(Q.transactions).flatMap(_.stream((account, account, status, status, limit), 128).compile.toList))

      def findForUpdate(s: Session[F], id: BankTransactionId): F[Option[BankTransaction]] =
        s.prepare(Q.txForUpdate).flatMap(_.option(id))

      def update(s: Session[F], tx: BankTransaction): F[Unit] =
        s.prepare(Q.updateTx).flatMap(_.execute(tx)).void

      def setRule(id: BankTransactionId, rule: Option[BankRuleId]): F[Unit] =
        pool.use(_.prepare(Q.setRule).flatMap(_.execute((rule, id)))).void
    }

  private object Q {

    private val ruleCols =
      sql"id, name, priority, apply_to, conjunction, account_id, contact_id, auto_categorize, active, created_at"

    val insertRule: Command[BankRule] =
      sql"INSERT INTO bank_rules ($ruleCols) VALUES ${ruleC.values}".command

    val insertCondition: Command[(BankRuleId, Int, RuleCondition)] =
      sql"INSERT INTO bank_rule_conditions (rule_id, position, field, operator, value) VALUES ($ruleIdC, $int4, $conditionC)".command

    val rules: Query[skunk.Void, BankRule] =
      sql"SELECT $ruleCols FROM bank_rules ORDER BY priority, created_at".query(ruleC)

    val conditions: Query[skunk.Void, (BankRuleId, RuleCondition)] =
      sql"SELECT rule_id, field, operator, value FROM bank_rule_conditions ORDER BY rule_id, position".query(ruleIdC *: conditionC)

    val setRuleActive: Command[(Boolean, BankRuleId)] =
      sql"UPDATE bank_rules SET active = $bool WHERE id = $ruleIdC".command

    private val txCols =
      sql"""id, account_id, tx_date, description, amount_minor, external_id, status, rule_id,
            category_account_id, contact_id, entry_id, posting_id, created_at"""

    val insertTx: Query[BankTransaction, BankTransaction] =
      sql"""INSERT INTO bank_transactions ($txCols) VALUES ${txC.values}
            ON CONFLICT (account_id, external_id) DO NOTHING
            RETURNING $txCols""".query(txC)

    val transactions: Query[(Option[AccountId], Option[AccountId], Option[BankTxStatus], Option[BankTxStatus], Int), BankTransaction] =
      sql"""SELECT $txCols FROM bank_transactions
            WHERE (${accountIdC.opt}::uuid IS NULL OR account_id = ${accountIdC.opt})
              AND (${statusC.opt}::varchar IS NULL OR status = ${statusC.opt})
            ORDER BY tx_date DESC, created_at DESC LIMIT $int4""".query(txC)

    val txForUpdate: Query[BankTransactionId, BankTransaction] =
      sql"SELECT $txCols FROM bank_transactions WHERE id = $txIdC FOR UPDATE".query(txC)

    val updateTx: Command[BankTransaction] =
      sql"""UPDATE bank_transactions
            SET status = $statusC, rule_id = ${ruleIdC.opt}, category_account_id = ${accountIdC.opt},
                contact_id = ${contactIdC.opt}, entry_id = ${entryIdC.opt}, posting_id = ${uuid.opt}
            WHERE id = $txIdC""".command
        .contramap[BankTransaction](t => (t.status, t.ruleId, t.categoryAccountId, t.contactId, t.entryId, t.postingId, t.id))

    val setRule: Command[(Option[BankRuleId], BankTransactionId)] =
      sql"UPDATE bank_transactions SET rule_id = ${ruleIdC.opt} WHERE id = $txIdC".command
  }
}
