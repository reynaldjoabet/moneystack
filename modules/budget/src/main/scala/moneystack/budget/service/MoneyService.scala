package moneystack.budget.service

import moneystack.domain.*
import moneystack.budget.domain.*
import moneystack.budget.db.{Catalog, Plans, Transactions, Wallets}
import moneystack.platform.FxQuote

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Day-to-day money: wallets (with read-only household sharing), categories,
 * payees, tags, exchange rates, transactions with payee matching and
 * automation rules, and CSV import/export. Owners write; members of a
 * shared wallet only read it.
 */
trait MoneyService[F[_]] {

  /** Owned wallets first, then wallets shared with the user. */
  def wallets(user: UserId): F[List[MoneyService.WalletView]]
  def createWallet(owner: UserId, b: MoneyService.NewWallet): F[Wallet]
  def setWalletArchived(owner: UserId, id: WalletId, archived: Boolean): F[Option[Wallet]]
  def shareWallet(owner: UserId, id: WalletId, member: UserId): F[Either[BudgetError, Unit]]
  def unshareWallet(owner: UserId, id: WalletId, member: UserId): F[Either[BudgetError, Unit]]

  def categories(owner: UserId): F[List[Category]]
  def createCategory(owner: UserId, b: MoneyService.NewCategory): F[Either[BudgetError, Category]]
  def setCategoryArchived(owner: UserId, id: CategoryId, archived: Boolean): F[Option[Category]]
  def payees(owner: UserId): F[List[Payee]]
  def createPayee(owner: UserId, b: MoneyService.NewPayee): F[Either[BudgetError, Payee]]
  def tags(owner: UserId): F[List[Tag]]
  def createTag(owner: UserId, name: Title): F[Tag]
  def addRates(rs: List[FxQuote]): F[Either[BudgetError, Int]]

  def transactions(user: UserId, filter: MoneyService.Filter): F[List[Transaction]]
  def addTransaction(owner: UserId, b: MoneyService.NewTransaction): F[Either[BudgetError, Transaction]]
  def updateTransaction(owner: UserId, id: TransactionId, b: MoneyService.NewTransaction): F[Either[BudgetError, Transaction]]
  def deleteTransaction(owner: UserId, id: TransactionId): F[Either[BudgetError, Unit]]

  def rules(owner: UserId): F[List[Rule]]
  def createRule(owner: UserId, b: MoneyService.NewRule): F[Either[BudgetError, Rule]]
  def deleteRule(owner: UserId, id: RuleId): F[Either[BudgetError, Unit]]

  /** What a rule would change in existing transactions; with `apply` it also saves the changes. */
  def runRuleOnHistory(owner: UserId, id: RuleId, apply: Boolean): F[Either[BudgetError, MoneyService.RuleRun]]

  def importCsv(owner: UserId, wallet: WalletId, csv: String, dryRun: Boolean): F[Either[BudgetError, MoneyService.ImportReport]]
  def exportCsv(user: UserId, from: LocalDate, to: LocalDate): F[String]
}

object MoneyService {

  final case class WalletView(wallet: Wallet, shared: Boolean)

  final case class NewWallet(name: Title, kind: WalletKind, currency: CurrencyCode, openingBalanceMinor: Option[Long])
  final case class NewCategory(name: Title, kind: CategoryKind, parentId: Option[CategoryId], colour: Option[String])
  final case class NewPayee(name: Title, aliases: Option[List[String]], defaultCategoryId: Option[CategoryId])
  final case class NewRule(
      name: Title,
      position: Option[Int],
      matchAll: Option[Boolean],
      conditions: List[Condition],
      actions: List[Action]
  )

  /**
   * Without `categoryId` an expense or income is categorised by its payee's
   * default, then by rules. `toAmountMinor` is needed for transfers between
   * currencies.
   */
  final case class NewTransaction(
      walletId: WalletId,
      kind: TxKind,
      amountMinor: Long,
      date: LocalDate,
      title: Title,
      note: Option[Body],
      categoryId: Option[CategoryId],
      payeeId: Option[PayeeId],
      tags: Option[List[TagId]],
      splits: Option[List[Split]],
      toWalletId: Option[WalletId],
      toAmountMinor: Option[Long],
      refundOf: Option[TransactionId],
      objectiveId: Option[ObjectiveId],
      excludedBudgets: Option[List[BudgetId]]
  )

  final case class Filter(
      wallet: Option[WalletId],
      category: Option[CategoryId],
      search: Option[String],
      from: LocalDate,
      to: LocalDate,
      limit: Int
  )

  final case class RuleRun(matched: Int, changed: Int, applied: Boolean, transactions: List[Transaction])

  final case class ImportReport(rows: Int, imported: Int, duplicates: Int, createdCategories: List[String], errors: List[String])

  /** Starter categories, after Cashew's and Expense Manager's defaults. */
  val DefaultCategories: List[(String, CategoryKind)] =
    List(
      "Groceries",
      "Dining",
      "Transport",
      "Shopping",
      "Bills & Utilities",
      "Housing",
      "Entertainment",
      "Health",
      "Travel",
      "Education",
      "Gifts",
      "Subscriptions",
      "Personal care",
      "Fees"
    ).map(_ -> CategoryKind.Expense) ++
      List("Salary", "Business", "Investments", "Gifts received", "Refunds", "Other income").map(_ -> CategoryKind.Income)

  def make[F[_]: Async](walletRepo: Wallets[F], catalog: Catalog[F], txRepo: Transactions[F], plans: Plans[F]): MoneyService[F] =
    new MoneyService[F] {

      private type R[A] = EitherT[F, BudgetError, A]
      private def lift[A](fa: F[A]): R[A]                          = EitherT.liftF(fa)
      private def check(ok: Boolean, err: => BudgetError): R[Unit] = EitherT.cond[F](ok, (), err)
      private def uuid                                             = Async[F].delay(UUID.randomUUID())

      // ---- wallets ----

      def wallets(user: UserId): F[List[WalletView]] =
        (walletRepo.owned(user), walletRepo.sharedWith(user)).mapN((o, s) => o.map(WalletView(_, false)) ++ s.map(WalletView(_, true)))

      def createWallet(owner: UserId, b: NewWallet): F[Wallet] =
        for {
          id  <- uuid
          now <- Async[F].delay(Instant.now())
          w   <- walletRepo.create(Wallet(
            WalletId.assume(id),
            owner,
            b.name,
            b.kind,
            b.currency,
            b.openingBalanceMinor.getOrElse(0L),
            archived = false,
            now
          ))
          _ <- seedCategories(owner)
        } yield w

      private def seedCategories(owner: UserId): F[Unit] =
        DefaultCategories.traverse_ { case (name, kind) =>
          uuid.flatMap(id =>
            catalog.ensureCategory(Category(CategoryId.assume(id), owner, Title.assume(name), kind, None, None, archived = false))
          )
        }

      def setWalletArchived(owner: UserId, id: WalletId, archived: Boolean): F[Option[Wallet]] = walletRepo.setArchived(owner, id, archived)

      private def ownedWallet(owner: UserId, id: WalletId): R[Wallet] =
        EitherT(walletRepo.owned(owner).map(_.find(_.id == id).toRight(BudgetError.WalletNotFound(id))))

      def shareWallet(owner: UserId, id: WalletId, member: UserId): F[Either[BudgetError, Unit]] =
        (for {
          _   <- ownedWallet(owner, id)
          _   <- check(member != owner, BudgetError.Invalid("a wallet can't be shared with its owner"))
          now <- lift(Async[F].delay(Instant.now()))
          _   <- lift(walletRepo.share(WalletShare(id, member, now)))
        } yield ()).value

      def unshareWallet(owner: UserId, id: WalletId, member: UserId): F[Either[BudgetError, Unit]] =
        (ownedWallet(owner, id) *> lift(walletRepo.unshare(id, member)).void).value

      // ---- catalogue ----

      def categories(owner: UserId): F[List[Category]] = catalog.categories(owner)

      def createCategory(owner: UserId, b: NewCategory): F[Either[BudgetError, Category]] =
        catalog.categories(owner).flatMap { cs =>
          val checked = for {
            _ <- Either.cond(
              !cs.exists(c => c.kind == b.kind && c.name == b.name),
              (),
              BudgetError.Invalid(s"a ${b.kind.key} category named ${b.name.value} exists")
            )
            _ <- b.parentId.traverse_ { pid =>
              cs.find(_.id == pid).toRight(BudgetError.CategoryNotFound(pid)).flatMap { p =>
                Either.cond(
                  p.kind == b.kind && p.parentId.isEmpty,
                  (),
                  BudgetError.Invalid("a parent must be a top-level category of the same kind")
                )
              }
            }
            _ <- Either.cond(b.colour.forall(_.matches("^#[0-9A-Fa-f]{6}$")), (), BudgetError.Invalid("colour must look like #1E88E5"))
          } yield ()
          checked.traverse(_ =>
            uuid.flatMap(id =>
              catalog.createCategory(Category(CategoryId.assume(id), owner, b.name, b.kind, b.parentId, b.colour, archived = false))
            )
          )
        }

      def setCategoryArchived(owner: UserId, id: CategoryId, archived: Boolean): F[Option[Category]] =
        catalog.setCategoryArchived(owner, id, archived)

      def payees(owner: UserId): F[List[Payee]] = catalog.payees(owner)

      def createPayee(owner: UserId, b: NewPayee): F[Either[BudgetError, Payee]] =
        (catalog.payees(owner), catalog.categories(owner)).tupled.flatMap { case (ps, cs) =>
          val checked = for {
            _ <- Either.cond(!ps.exists(_.name == b.name), (), BudgetError.Invalid(s"payee ${b.name.value} exists"))
            _ <- b.defaultCategoryId.traverse_(id => cs.find(_.id == id).toRight(BudgetError.CategoryNotFound(id)))
            _ <- Either.cond(
              b.aliases.forall(_.forall(a => a.trim.nonEmpty && a.length <= 200)),
              (),
              BudgetError.Invalid("aliases must be 1-200 characters")
            )
          } yield ()
          checked.traverse(_ =>
            uuid.flatMap(id =>
              catalog.createPayee(Payee(PayeeId.assume(id), owner, b.name, b.aliases.getOrElse(Nil).distinct, b.defaultCategoryId))
            )
          )
        }

      def tags(owner: UserId): F[List[Tag]] = catalog.tags(owner)

      def createTag(owner: UserId, name: Title): F[Tag] =
        catalog.tags(owner).flatMap(ts =>
          ts.find(_.name == name).fold(uuid.flatMap(id => catalog.createTag(Tag(TagId.assume(id), owner, name))))(Async[F].pure)
        )

      def addRates(rs: List[FxQuote]): F[Either[BudgetError, Int]] =
        if (rs.exists(r => r.from == r.to || r.rate <= 0))
          Async[F].pure(Left(BudgetError.Invalid("rates need two currencies and a positive rate")))
        else catalog.upsertRates(rs).as(Right(rs.size))

      // ---- transactions ----

      private def accessible(user: UserId): F[List[Wallet]] = wallets(user).map(_.map(_.wallet))

      def transactions(user: UserId, f: Filter): F[List[Transaction]] =
        accessible(user).flatMap { ws =>
          val ids = f.wallet.fold(ws.map(_.id))(w => ws.map(_.id).filter(_ == w))
          txRepo.forWallets(ids).map(_.filter { t =>
            !t.date.isBefore(f.from) && !t.date.isAfter(f.to) &&
            f.category.forall(c => t.categoryId.contains(c) || t.splits.exists(_.categoryId == c)) &&
            f.search.forall(q =>
              (t.title.value: String).toLowerCase.contains(q.toLowerCase) ||
                t.note.exists(n => (n.value: String).toLowerCase.contains(q.toLowerCase))
            )
          }.sortBy(t => (t.date, t.createdAt)).reverse.take(f.limit))
        }

      /** Validates a draft against the owner's data, then fills in payee, category and rule effects. */
      private def prepare(
          owner: UserId,
          id: TransactionId,
          b: NewTransaction,
          createdAt: Instant,
          existingKey: Option[String]
      ): R[Transaction] =
        for {
          ws     <- lift(walletRepo.owned(owner))
          cs     <- lift(catalog.categories(owner))
          ps     <- lift(catalog.payees(owner))
          ts     <- lift(catalog.tags(owner))
          objs   <- lift(plans.objectives(owner))
          wallet <- EitherT.fromOption[F](ws.find(_.id == b.walletId), BudgetError.WalletNotFound(b.walletId): BudgetError)
          _      <- check(b.amountMinor > 0, BudgetError.Invalid("amount must be positive"))
          splits  = b.splits.getOrElse(Nil)
          catKind = if (b.kind == TxKind.Income) CategoryKind.Income else CategoryKind.Expense
          _ <- (b.categoryId.toList ++ splits.map(_.categoryId)).traverse_ { cid =>
            EitherT.fromOption[F](cs.find(_.id == cid), BudgetError.CategoryNotFound(cid): BudgetError).flatMap(c =>
              check(b.kind != TxKind.Transfer && c.kind == catKind, BudgetError.Invalid(s"category ${c.name.value} is for ${c.kind.key}"))
            )
          }
          _ <- check(
            splits.isEmpty || (splits.forall(_.amountMinor > 0) && splits.map(_.amountMinor).sum == b.amountMinor && b.categoryId.isEmpty),
            BudgetError.Invalid("splits must be positive, add up to the amount, and replace the category")
          )
          _ <- b.payeeId.traverse_(p => check(ps.exists(_.id == p), BudgetError.PayeeNotFound(p)))
          _ <- b.tags.getOrElse(Nil).traverse_(t => check(ts.exists(_.id == t), BudgetError.TagNotFound(t)))
          _ <- b.objectiveId.traverse_(o => check(objs.exists(_.id == o), BudgetError.ObjectiveNotFound(o)))
          _ <- b.kind match {
            case TxKind.Transfer =>
              for {
                toId <- EitherT.fromOption[F](b.toWalletId, BudgetError.Invalid("a transfer needs toWalletId"): BudgetError)
                to   <- EitherT.fromOption[F](ws.find(_.id == toId), BudgetError.WalletNotFound(toId): BudgetError)
                _    <- check(to.id != wallet.id, BudgetError.Invalid("a transfer needs two different wallets"))
                _    <- check(
                  to.currency == wallet.currency || b.toAmountMinor.exists(_ > 0),
                  BudgetError.Invalid("a transfer between currencies needs toAmountMinor")
                )
              } yield ()
            case _ =>
              check(b.toWalletId.isEmpty && b.toAmountMinor.isEmpty, BudgetError.Invalid("only transfers have a destination wallet"))
          }
          _ <- b.refundOf.traverse_ { r =>
            for {
              _    <- check(b.kind == TxKind.Income, BudgetError.Invalid("a refund is income"))
              mine <- lift(txRepo.forWallets(ws.map(_.id)))
              _    <-
                check(mine.exists(t => t.id == r && t.kind == TxKind.Expense), BudgetError.Invalid("refundOf must be one of your expenses"))
            } yield ()
          }
          rules <- lift(plans.rules(owner))
        } yield {
          val payee         = b.payeeId.orElse(Option.when(b.kind != TxKind.Transfer)(Payees.find(ps, b.title.value).map(_.id)).flatten)
          val (cat, source) = b.categoryId.map(_ -> CategorySource.Manual)
            .orElse(Option.when(
              b.kind != TxKind.Transfer && splits.isEmpty
            )(payee.flatMap(p => ps.find(_.id == p)).flatMap(_.defaultCategoryId)
              .filter(c => cs.exists(x => x.id == c && x.kind == catKind)).map(_ -> CategorySource.Payee)).flatten)
            .fold[(Option[CategoryId], CategorySource)]((None, CategorySource.Unset))((c, s) => (Some(c), s))
          val draft = Transaction(
            id,
            owner,
            b.walletId,
            b.kind,
            b.amountMinor,
            b.date,
            b.title,
            b.note,
            cat,
            source,
            payee,
            b.tags.getOrElse(Nil).distinct,
            splits,
            b.toWalletId,
            b.toAmountMinor.orElse(Option.when(b.kind == TxKind.Transfer)(b.amountMinor)),
            b.refundOf,
            b.objectiveId,
            None,
            None,
            b.excludedBudgets.getOrElse(Nil).distinct,
            existingKey,
            owner,
            createdAt
          )
          Rules.run(rules, draft)._1
        }

      def addTransaction(owner: UserId, b: NewTransaction): F[Either[BudgetError, Transaction]] =
        (for {
          id  <- lift(uuid)
          now <- lift(Async[F].delay(Instant.now()))
          t   <- prepare(owner, TransactionId.assume(id), b, now, None)
          out <- lift(txRepo.insert(t))
        } yield out).value

      private def findOwned(owner: UserId, id: TransactionId): R[Transaction] =
        EitherT(walletRepo.owned(owner).flatMap(ws => txRepo.forWallets(ws.map(_.id))).map(_.find(t => t.id == id && t.ownerId == owner)
          .toRight(BudgetError.TransactionNotFound(id))))

      def updateTransaction(owner: UserId, id: TransactionId, b: NewTransaction): F[Either[BudgetError, Transaction]] =
        (for {
          old <- findOwned(owner, id)
          t   <- prepare(owner, id, b, old.createdAt, old.importKey)
          out <- EitherT.fromOptionF(
            txRepo.replace(t.copy(scheduleId = old.scheduleId, dueDate = old.dueDate)),
            BudgetError.TransactionNotFound(id): BudgetError
          )
        } yield out).value

      def deleteTransaction(owner: UserId, id: TransactionId): F[Either[BudgetError, Unit]] =
        (for {
          old <- findOwned(owner, id)
          _   <- lift(txRepo.delete(id, List(old.walletId)))
        } yield ()).value

      // ---- rules ----

      def rules(owner: UserId): F[List[Rule]] = plans.rules(owner)

      def createRule(owner: UserId, b: NewRule): F[Either[BudgetError, Rule]] =
        (for {
          _  <- EitherT.fromEither[F](Rules.validate(b.conditions, b.actions))
          cs <- lift(catalog.categories(owner))
          ps <- lift(catalog.payees(owner))
          ts <- lift(catalog.tags(owner))
          _  <- b.actions.traverse_ {
            case Action.SetCategory(c) => check(cs.exists(_.id == c), BudgetError.CategoryNotFound(c))
            case Action.SetPayee(p)    => check(ps.exists(_.id == p), BudgetError.PayeeNotFound(p))
            case Action.AddTags(ids)   => ids.traverse_(t => check(ts.exists(_.id == t), BudgetError.TagNotFound(t)))
            case Action.SetNote(_, _)  => EitherT.rightT[F, BudgetError](())
          }
          rs   <- lift(plans.rules(owner))
          id   <- lift(uuid)
          now  <- lift(Async[F].delay(Instant.now()))
          rule <- lift(plans.createRule(Rule(
            RuleId.assume(id),
            owner,
            b.name,
            b.position.getOrElse(rs.map(_.position).maxOption.fold(1)(_ + 1)),
            enabled = true,
            b.matchAll.getOrElse(true),
            b.conditions,
            b.actions,
            0L,
            now
          )))
        } yield rule).value

      def deleteRule(owner: UserId, id: RuleId): F[Either[BudgetError, Unit]] =
        plans.deleteRule(owner, id).map(found => Either.cond(found, (), BudgetError.RuleNotFound(id)))

      def runRuleOnHistory(owner: UserId, id: RuleId, apply: Boolean): F[Either[BudgetError, RuleRun]] =
        (for {
          rule <- EitherT(plans.rules(owner).map(_.find(_.id == id).toRight(BudgetError.RuleNotFound(id))))
          ws   <- lift(walletRepo.owned(owner))
          txs  <- lift(txRepo.forWallets(ws.map(_.id)))
          matched = txs.filter(t => t.ownerId == owner && Rules.matches(rule, t))
          changed = matched.map(Rules.applyTo(rule, _)).zip(matched).collect { case (after, before) if after != before => after }
          _ <- if (apply) lift(changed.traverse_(txRepo.replace) *> plans.recordMatches(rule.id, changed.size.toLong))
          else EitherT.rightT[F, BudgetError](())
        } yield RuleRun(matched.size, changed.size, apply, changed)).value

      // ---- CSV ----

      def importCsv(owner: UserId, wallet: WalletId, csv: String, dryRun: Boolean): F[Either[BudgetError, ImportReport]] = {
        val parsed = BudgetCsv.parse(csv)
        (for {
          w        <- ownedWallet(owner, wallet)
          _        <- check(parsed.errors.isEmpty, BudgetError.ImportFailed(parsed.errors))
          existing <- lift(txRepo.forWallets(List(w.id)))
          known = existing.flatMap(_.importKey).toSet
          fresh = parsed.rows.distinctBy(_.key(w.id)).filterNot(r => known(r.key(w.id)))
          cs <- lift(catalog.categories(owner))
          wanted =
            fresh.flatMap(r => r.category.map(_ -> (if (r.kind == TxKind.Income) CategoryKind.Income else CategoryKind.Expense))).distinct
          missing = wanted.filterNot { case (n, k) => cs.exists(c => (c.name.value: String).equalsIgnoreCase(n) && c.kind == k) }
          _ <- if (dryRun) EitherT.rightT[F, BudgetError](())
          else lift(missing.traverse_ { case (n, k) =>
            uuid.flatMap(id =>
              catalog.ensureCategory(Category(CategoryId.assume(id), owner, Title.assume(n.take(200)), k, None, None, archived = false))
            )
          })
          cats  <- lift(catalog.categories(owner))
          ps    <- lift(catalog.payees(owner))
          rules <- lift(plans.rules(owner))
          now   <- lift(Async[F].delay(Instant.now()))
          rows  <- lift(fresh.traverse { r =>
            val kind  = if (r.kind == TxKind.Income) CategoryKind.Income else CategoryKind.Expense
            val cat   = r.category.flatMap(n => cats.find(c => (c.name.value: String).equalsIgnoreCase(n) && c.kind == kind)).map(_.id)
            val payee =
              r.payee.flatMap(n => ps.find(p => (p.name.value: String).equalsIgnoreCase(n))).orElse(Payees.find(ps, r.title.value))
            uuid.map { id =>
              val t = Transaction(
                TransactionId.assume(id),
                owner,
                w.id,
                r.kind,
                r.amountMinor,
                r.date,
                r.title,
                r.note,
                cat.orElse(payee.flatMap(_.defaultCategoryId)),
                if (cat.isDefined) CategorySource.Manual
                else if (payee.flatMap(_.defaultCategoryId).isDefined) CategorySource.Payee else CategorySource.Unset,
                payee.map(_.id),
                Nil,
                Nil,
                None,
                None,
                None,
                None,
                None,
                None,
                Nil,
                Some(r.key(w.id)),
                owner,
                now
              )
              Rules.run(rules, t)._1
            }
          })
          inserted <- if (dryRun) EitherT.rightT[F, BudgetError](rows.size)
          else lift(rows.traverse(t => txRepo.insert(t).attempt).map(_.count(_.isRight)))
        } yield ImportReport(parsed.rows.size, inserted, parsed.rows.size - inserted, missing.map(_._1), Nil)).value
      }

      def exportCsv(user: UserId, from: LocalDate, to: LocalDate): F[String] =
        for {
          ws  <- accessible(user)
          txs <- txRepo.forWallets(ws.map(_.id)).map(_.filter(t => !t.date.isBefore(from) && !t.date.isAfter(to)))
          cs  <- ws.map(_.ownerId).distinct.flatTraverse(catalog.categories)
          ps  <- ws.map(_.ownerId).distinct.flatTraverse(catalog.payees)
          ts  <- ws.map(_.ownerId).distinct.flatTraverse(catalog.tags)
        } yield BudgetCsv.write(
          txs,
          ws.map(w => w.id -> w).toMap,
          cs.map(c => c.id -> c).toMap,
          ps.map(p => p.id -> p).toMap,
          ts.map(t => t.id -> t).toMap
        )
    }
}
