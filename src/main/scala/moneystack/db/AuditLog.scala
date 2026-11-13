package moneystack.db

import moneystack.domain.*

import cats.effect.*
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.codec.all.{int4, int8, timestamptz, varchar}
import skunk.implicits.*

import java.time.Instant

/** An append-only record of security events: logins, password and role changes. */
trait AuditLog[F[_]] {

  /** Never fails: an event that can't be saved is logged instead, so auditing can't take sign-in down. */
  def record(
      action: String,
      origin: AuditLog.Origin,
      actor: Option[UserId],
      subject: Option[UserId],
      detail: Option[String] = None
  ): F[Unit]

  /** Newest first, optionally only those about one user. */
  def recent(subject: Option[UserId], limit: Int): F[List[AuditLog.Event]]
}

object AuditLog {

  val Signup                 = "signup"
  val LoginSucceeded         = "login.succeeded"
  val LoginFailed            = "login.failed"
  val PasswordChanged        = "password.changed"
  val PasswordChangeRefused  = "password.change_refused"
  val PasswordResetRequested = "password.reset_requested"
  val PasswordReset          = "password.reset"
  val PasswordResetRefused   = "password.reset_refused"
  val TokensRevoked          = "tokens.revoked"
  val RoleChanged            = "role.changed"

  /**
   * Where a request came from. `forwardedFor` is the `X-Forwarded-For` header as received: it names the real client
   * only when the gateway in front sets it, and is whatever the caller sent otherwise.
   */
  final case class Origin(requestId: String, forwardedFor: Option[String])

  final case class Event(
      id: Long,
      at: Instant,
      action: String,
      actorId: Option[UserId],
      subjectId: Option[UserId],
      requestId: Option[String],
      forwardedFor: Option[String],
      detail: Option[String]
  )

  def make[F[_]: Concurrent](pool: Resource[F, Session[F]], logger: Logger[F]): AuditLog[F] = new AuditLog[F] {

    def record(action: String, origin: Origin, actor: Option[UserId], subject: Option[UserId], detail: Option[String]): F[Unit] =
      pool.use(_.prepare(Q.insert).flatMap(
        _.execute((action, actor, subject, origin.requestId.take(64), origin.forwardedFor.map(_.take(200)), detail.map(_.take(200))))
      )).void
        .handleErrorWith(e => logger.error(e)(s"could not save audit event $action (request ${origin.requestId})"))

    def recent(subject: Option[UserId], limit: Int): F[List[Event]] =
      pool.use(_.prepare(Q.recent).flatMap(_.stream((subject, subject, limit), 128).compile.toList))
  }

  private object Q {

    private val user = Codecs.userId.opt

    val insert: Command[(String, Option[UserId], Option[UserId], String, Option[String], Option[String])] =
      sql"""INSERT INTO audit_events (action, actor_id, subject_id, request_id, forwarded_for, detail)
            VALUES (${varchar(40)}, $user, $user, ${varchar(64)}, ${varchar(200).opt}, ${varchar(200).opt})""".command

    val recent: Query[(Option[UserId], Option[UserId], Int), Event] =
      sql"""SELECT id, at, action, actor_id, subject_id, request_id, forwarded_for, detail FROM audit_events
            WHERE ($user::uuid IS NULL OR subject_id = $user) ORDER BY id DESC LIMIT $int4"""
        .query(int8 *: timestamptz.map(_.toInstant) *: varchar(40) *: user *: user *: varchar(64).opt *: varchar(200).opt *:
          varchar(200).opt)
        .to[Event]
  }
}
