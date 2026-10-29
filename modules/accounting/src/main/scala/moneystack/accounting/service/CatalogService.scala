package moneystack.accounting.service

import moneystack.domain.*
import moneystack.accounting.domain.*
import moneystack.accounting.db.{Contacts, Items, TaxRates}
import moneystack.accounting.service.LedgerService.SystemAccounts

import cats.effect.*
import cats.syntax.all.*

import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.UUID

/** Reference data the trading flows rely on: contacts, tax rates and items. */
trait CatalogService[F[_]] {
  def contacts(kind: Option[ContactKind]): F[List[Contact]]
  def createContact(b: CatalogService.NewContact): F[Either[LedgerError, Contact]]

  /** The contact of `kind` linked to an app user, created on first use. */
  def contactForUser(kind: ContactKind, userId: UserId, name: Title, email: Option[Email], currency: CurrencyCode): F[Contact]
  def setContactActive(id: ContactId, active: Boolean): F[Option[Contact]]

  def taxRates: F[List[TaxRate]]
  def createTaxRate(b: CatalogService.NewTaxRate): F[Either[LedgerError, TaxRate]]
  def setTaxRateActive(id: TaxRateId, active: Boolean): F[Option[TaxRate]]

  def items: F[List[Item]]
  def createItem(b: CatalogService.NewItem): F[Either[LedgerError, Item]]
  def setItemActive(id: ItemId, active: Boolean): F[Option[Item]]
}

object CatalogService {

  final case class NewContact(kind: ContactKind, name: Title, email: Option[Email], currency: Option[CurrencyCode])

  final case class NewTaxRate(code: TaxCode, name: Title, rateBps: Bps)

  /** Accounts default to Sales / COGS or G&A / Inventory from the system chart. */
  final case class NewItem(
      sku: Sku,
      name: Title,
      kind: ItemKind,
      sellPriceMinor: Option[PriceMinor],
      costPriceMinor: Option[PriceMinor],
      incomeAccountId: Option[AccountId],
      expenseAccountId: Option[AccountId],
      inventoryAccountId: Option[AccountId]
  )

  def make[F[_]: Async](posting: Posting[F], contactRepo: Contacts[F], taxRateRepo: TaxRates[F], itemRepo: Items[F]): CatalogService[F] =
    new CatalogService[F] {

      def contacts(kind: Option[ContactKind]): F[List[Contact]] = contactRepo.list(kind)

      def createContact(b: NewContact): F[Either[LedgerError, Contact]] =
        for {
          id  <- Async[F].delay(ContactId.assume(UUID.randomUUID()))
          now <- Async[F].delay(Instant.now())
          c   <-
            contactRepo.create(Contact(id, b.kind, b.name, b.email, b.currency.getOrElse(posting.baseCurrency), None, active = true, now))
        } yield Right(c)

      def contactForUser(kind: ContactKind, userId: UserId, name: Title, email: Option[Email], currency: CurrencyCode): F[Contact] = {
        // "wealth." is a fixed namespace from the project's first name: changing it would change existing ids.
        val id = ContactId.assume(UUID.nameUUIDFromBytes(s"wealth.user.${kind.key}.${userId.value}".getBytes(UTF_8)))
        for {
          now <- Async[F].delay(Instant.now())
          _   <- contactRepo.ensure(Contact(id, kind, name, email, currency, Some(userId), active = true, now))
          c   <- contactRepo.find(id).flatMap(Async[F].fromOption(_, new IllegalStateException(s"contact ${id.value} vanished")))
        } yield c
      }

      def setContactActive(id: ContactId, active: Boolean): F[Option[Contact]] = contactRepo.setActive(id, active)

      def taxRates: F[List[TaxRate]] = taxRateRepo.list()

      def createTaxRate(b: NewTaxRate): F[Either[LedgerError, TaxRate]] =
        taxRateRepo.list().flatMap { existing =>
          if (existing.exists(_.code == b.code)) Async[F].pure(Left(LedgerError.DuplicateCode("tax rate", b.code.value)))
          else
            for {
              id  <- Async[F].delay(TaxRateId.assume(UUID.randomUUID()))
              now <- Async[F].delay(Instant.now())
              t   <- taxRateRepo.create(TaxRate(id, b.code, b.name, b.rateBps, active = true, now))
            } yield Right(t)
        }

      def setTaxRateActive(id: TaxRateId, active: Boolean): F[Option[TaxRate]] = taxRateRepo.setActive(id, active)

      def items: F[List[Item]] = itemRepo.list()

      def createItem(b: NewItem): F[Either[LedgerError, Item]] =
        (itemRepo.list(), posting.chart).tupled.flatMap { case (existing, chart) =>
          val inventory = b.kind == ItemKind.Inventory

          def account(id: AccountId, expected: String)(ok: AccountType => Boolean): Either[LedgerError, AccountId] =
            chart.get(id) match {
              case None                          => Left(LedgerError.UnknownAccount(id))
              case Some(a) if !ok(a.accountType) => Left(LedgerError.WrongAccountType(a.code, expected))
              case Some(_)                       => Right(id)
            }

          val resolved: Either[LedgerError, (AccountId, AccountId, Option[AccountId])] = for {
            _       <- Either.cond(!existing.exists(_.sku == b.sku), (), LedgerError.DuplicateCode("item", b.sku.value))
            income  <- account(b.incomeAccountId.getOrElse(SystemAccounts.Sales.id), "income")(_.root == RootType.Income)
            expense <- account(
              b.expenseAccountId.getOrElse(if (inventory) SystemAccounts.CostOfGoodsSold.id else SystemAccounts.GeneralExpenses.id),
              if (inventory) "cost of sales or expense" else "expense or asset"
            )(t => t.root == RootType.Expense || (!inventory && t.root == RootType.Asset))
            stock <- if (inventory)
              account(b.inventoryAccountId.getOrElse(SystemAccounts.Inventory.id), "inventory")(_ == AccountType.Inventory).map(Some(_))
            else Right(None)
          } yield (income, expense, stock)

          resolved.traverse { case (income, expense, stock) =>
            for {
              id  <- Async[F].delay(ItemId.assume(UUID.randomUUID()))
              now <- Async[F].delay(Instant.now())
              i   <- itemRepo.create(Item(
                id,
                b.sku,
                b.name,
                b.kind,
                b.sellPriceMinor,
                b.costPriceMinor,
                income,
                expense,
                stock,
                active = true,
                now
              ))
            } yield i
          }
        }

      def setItemActive(id: ItemId, active: Boolean): F[Option[Item]] = itemRepo.setActive(id, active)
    }
}
