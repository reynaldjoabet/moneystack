package moneystack.db

import moneystack.domain.*

import cats.effect.*
import cats.syntax.all.*
import skunk.*
import skunk.implicits.*

trait Users[F[_]] {
  def create(u: User): F[User]
  def find(id: UserId): F[Option[User]]
  def findByEmail(email: Email): F[Option[User]]
  def linkTelegram(id: UserId, chatId: TelegramChatId): F[Unit]

  /** The updated user, or `None` when there is no such user. */
  def setRole(id: UserId, role: Role): F[Option[User]]

  /** Replaces the password hash and revokes every token issued so far. */
  def setPassword(id: UserId, hash: PasswordHash): F[Option[User]]

  /** Revokes every token issued so far ("sign out everywhere"), returning the user at their new token version. */
  def revokeTokens(id: UserId): F[Option[User]]
}

object Users {

  import Codecs.{user as userC, userId as userIdC, email as emailC, passwordHash as hashC, role as roleC, telegramChat}

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

      def setRole(id: UserId, role: Role): F[Option[User]] =
        pool.use(_.prepare(Q.setRole).flatMap(_.option((role, id))))

      def setPassword(id: UserId, hash: PasswordHash): F[Option[User]] =
        pool.use(_.prepare(Q.setPassword).flatMap(_.option((hash, id))))

      def revokeTokens(id: UserId): F[Option[User]] =
        pool.use(_.prepare(Q.revokeTokens).flatMap(_.option(id)))
    }

  private object Q {
    private val cols = sql"id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at, token_version"

    val insert: Query[User, User] =
      sql"""INSERT INTO users (id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at, token_version)
            VALUES ${userC.values}
            RETURNING id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at, token_version""".query(userC)

    val byId: Query[UserId, User] =
      sql"""SELECT id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at, token_version
            FROM users WHERE id = $userIdC""".query(userC)

    val byEmail: Query[Email, User] =
      sql"""SELECT id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at, token_version
            FROM users WHERE email = $emailC""".query(userC)

    val linkTg: Command[(TelegramChatId, UserId)] =
      sql"UPDATE users SET telegram_chat_id = $telegramChat WHERE id = $userIdC".command

    val setRole: Query[(Role, UserId), User] =
      sql"""UPDATE users SET role = $roleC WHERE id = $userIdC
            RETURNING id, email, password_hash, display_name, role, locale, telegram_chat_id, created_at, token_version""".query(userC)

    val setPassword: Query[(PasswordHash, UserId), User] =
      sql"""UPDATE users SET password_hash = $hashC, token_version = token_version + 1 WHERE id = $userIdC
            RETURNING $cols""".query(userC)

    val revokeTokens: Query[UserId, User] =
      sql"UPDATE users SET token_version = token_version + 1 WHERE id = $userIdC RETURNING $cols".query(userC)
  }
}
