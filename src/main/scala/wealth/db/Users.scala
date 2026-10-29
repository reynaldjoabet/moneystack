package wealth.db

import wealth.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait Users[F[_]] {
  def create(u: User): F[User]
  def find(id: UserId): F[Option[User]]
  def findByEmail(email: Email): F[Option[User]]
  def linkTelegram(id: UserId, chatId: TelegramChatId): F[Unit]
}

object Users {

  import Codecs.{user as userC, userId as userIdC, email as emailC, telegramChat}

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]]): Users[F] =
    new Users[F] {
      def create(u: User): F[User] =
        pool.use(_.prepare(Q.insert).flatMap(_.unique(u)))

      def find(id: UserId): F[Option[User]] =
        pool.use(_.prepare(Q.byId).flatMap(_.option(id)))

      def findByEmail(e: Email): F[Option[User]] =
        pool.use(_.prepare(Q.byEmail).flatMap(_.option(e)))

      def linkTelegram(id: UserId, chatId: TelegramChatId): F[Unit] =
        pool.use(_.prepare(Q.linkTg).flatMap(_.execute((chatId, id)))).void
    }

  private object Q {
    val insert: Query[User, User] =
      sql"""INSERT INTO users (id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at)
            VALUES $userC
            RETURNING id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at""".query(userC)

    val byId: Query[UserId, User] =
      sql"""SELECT id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at
            FROM users WHERE id = $userIdC""".query(userC)

    val byEmail: Query[Email, User] =
      sql"""SELECT id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at
            FROM users WHERE email = $emailC""".query(userC)

    val linkTg: Command[(TelegramChatId, UserId)] =
      sql"UPDATE users SET telegram_chat_id = $telegramChat WHERE id = $userIdC".command
  }
}
