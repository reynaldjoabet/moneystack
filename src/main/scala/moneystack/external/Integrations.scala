package moneystack.external

import moneystack.domain.*

import cats.effect.*
import cats.syntax.all.*

/**
 * The integration sprawl from the brief, behind small traits. Every one ships
 * with a `sandbox` no-op so the system runs end-to-end without credentials.
 * Replace each `sandbox` with a real client when you have the keys.
 */

// ---------- Salesforce CRM ----------

trait Crm[F[_]] {
  def upsertContact(userId: UserId, email: Email, displayName: DisplayName): F[Unit]
  def recordSubscriptionChange(userId: UserId, sub: Subscription): F[Unit]
  def recordPayment(userId: UserId, payment: Payment): F[Unit]
}

object Crm {
  def sandbox[F[_]: Sync]: F[Crm[F]] = Sync[F].pure(new Crm[F] {
    def upsertContact(userId: UserId, email: Email, displayName: DisplayName): F[Unit] = Sync[F].unit
    def recordSubscriptionChange(userId: UserId, sub: Subscription): F[Unit]           = Sync[F].unit
    def recordPayment(userId: UserId, payment: Payment): F[Unit]                       = Sync[F].unit
  })
}

// ---------- Thomson Reuters ONESOURCE (tax) ----------

trait Tax[F[_]] {
  def submit1099(userId: UserId, year: Int): F[Tax.Receipt]
  def fetchStatement(receipt: Tax.Receipt): F[String] // URL to the rendered statement
}

object Tax {
  final case class Receipt(ref: String)
  def sandbox[F[_]: Sync]: F[Tax[F]] = Sync[F].pure(new Tax[F] {
    def submit1099(userId: UserId, year: Int): F[Receipt] =
      Sync[F].delay(Receipt("ts_" + java.util.UUID.randomUUID().toString.take(16)))
    def fetchStatement(r: Receipt): F[String] =
      Sync[F].pure(s"https://onesource.example/statements/${r.ref}")
  })
}

// ---------- FinancialForce accounting ----------

trait Accounting[F[_]] {
  def postRevenue(userId: UserId, payment: Payment): F[Unit]
  def revenueReport(year: Int, month: Int): F[List[(UserId, PriceMinor)]]
}

object Accounting {
  def sandbox[F[_]: Sync]: F[Accounting[F]] = Sync[F].pure(new Accounting[F] {
    def postRevenue(userId: UserId, payment: Payment): F[Unit]              = Sync[F].unit
    def revenueReport(year: Int, month: Int): F[List[(UserId, PriceMinor)]] = Sync[F].pure(Nil)
  })
}

// ---------- Telegram bot ----------

trait Telegram[F[_]] {
  def sendMessage(chatId: TelegramChatId, text: String): F[Unit]
  def broadcastToInvestors(chatIds: List[TelegramChatId], text: String): F[Unit]
}

object Telegram {
  def sandbox[F[_]: Sync](log: String => F[Unit]): F[Telegram[F]] =
    Sync[F].pure(new Telegram[F] {
      def sendMessage(chatId: TelegramChatId, text: String): F[Unit] =
        log(s"[telegram chat=${chatId.value}] $text")
      def broadcastToInvestors(chatIds: List[TelegramChatId], text: String): F[Unit] =
        chatIds.foldLeft(Sync[F].unit)((acc, id) => acc.flatMap(_ => sendMessage(id, text)))
    })
}

// ---------- Native push notifications ----------

trait Push[F[_]] {
  def send(userId: UserId, title: Title, body: Body): F[Unit]
}

object Push {
  def sandbox[F[_]: Sync](log: String => F[Unit]): F[Push[F]] = Sync[F].pure(new Push[F] {
    def send(userId: UserId, title: Title, body: Body): F[Unit] =
      log(s"[push user=${userId.value}] ${title.value} - ${body.value}")
  })
}

// ---------- Error monitoring (Sentry) ----------

trait ErrorMonitoring[F[_]] {
  def captureException(err: Throwable, tags: Map[String, String]): F[Unit]
  def captureMessage(msg: String, level: String): F[Unit]
}

object ErrorMonitoring {
  def sandbox[F[_]: Sync]: F[ErrorMonitoring[F]] = Sync[F].pure(new ErrorMonitoring[F] {
    def captureException(err: Throwable, tags: Map[String, String]): F[Unit] = Sync[F].unit
    def captureMessage(msg: String, level: String): F[Unit]                  = Sync[F].unit
  })
}

// ---------- Jira incident / support ticket sink ----------

trait SupportSink[F[_]] {
  def openTicket(userId: UserId, summary: Title, description: Body): F[String] // returns issue key
}

object SupportSink {
  def sandbox[F[_]: Sync]: F[SupportSink[F]] = Sync[F].pure(new SupportSink[F] {
    def openTicket(userId: UserId, summary: Title, description: Body): F[String] =
      Sync[F].delay("MONEYSTACK-" + java.util.UUID.randomUUID().toString.take(8).toUpperCase)
  })
}
