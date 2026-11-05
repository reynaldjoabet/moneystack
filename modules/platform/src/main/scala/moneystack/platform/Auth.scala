package moneystack.platform

import moneystack.domain.UserId

import org.http4s.Request

/** Why a request was refused: no valid credentials (401), or a caller who may not do this (403). */
enum AuthFailure {
  case Unauthenticated, Forbidden
}

/** Resolves the caller of a request, or why it is refused. The host app decides what qualifies. */
type Auth[F[_]] = Request[F] => F[Either[AuthFailure, UserId]]
