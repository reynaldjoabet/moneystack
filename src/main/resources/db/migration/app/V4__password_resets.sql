-- One-time password reset tokens. Only a SHA-256 of the token is stored, so a copy of this table can't reset anyone.
CREATE TABLE IF NOT EXISTS password_resets (
    token_hash  CHAR(64)    PRIMARY KEY,
    user_id     UUID        NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    expires_at  TIMESTAMPTZ NOT NULL,
    used_at     TIMESTAMPTZ,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS password_resets_user_idx ON password_resets(user_id);
