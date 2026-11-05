-- Access tokens carry the version they were issued at; bumping it (password change, "sign out everywhere")
-- invalidates every token issued before.
ALTER TABLE users ADD COLUMN IF NOT EXISTS token_version INTEGER NOT NULL DEFAULT 0;
