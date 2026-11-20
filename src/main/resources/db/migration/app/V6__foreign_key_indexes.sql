-- Indexes on the referencing side of foreign keys into large tables. Postgres doesn't create these: without one,
-- every delete or key change in the parent scans the whole child table, holding its locks meanwhile.
-- Nullable keys get a partial index, which is smaller and still serves the foreign key check.
--
-- CONCURRENTLY builds each index without blocking writes, so it can't run in a transaction (see the .conf next to
-- this file). If a build fails, Postgres leaves an INVALID index that IF NOT EXISTS would then skip: drop it, run
-- `flyway repair`, and deploy again. Find them with
--   SELECT indexrelid::regclass FROM pg_index WHERE NOT indisvalid;

CREATE INDEX CONCURRENTLY IF NOT EXISTS payments_subscription_id_fk_idx ON payments(subscription_id) WHERE subscription_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS recommendations_published_by_fk_idx ON recommendations(published_by);
CREATE INDEX CONCURRENTLY IF NOT EXISTS alerts_user_id_fk_idx ON alerts(user_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS news_items_author_id_fk_idx ON news_items(author_id) WHERE author_id IS NOT NULL;
