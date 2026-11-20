-- Indexes on the referencing side of foreign keys into large tables. Postgres doesn't create these: without one,
-- every delete or key change in the parent scans the whole child table, holding its locks meanwhile.
-- Nullable keys get a partial index, which is smaller and still serves the foreign key check.
--
-- CONCURRENTLY builds each index without blocking writes, so it can't run in a transaction (see the .conf next to
-- this file). If a build fails, Postgres leaves an INVALID index that IF NOT EXISTS would then skip: drop it, run
-- `flyway repair`, and deploy again. Find them with
--   SELECT indexrelid::regclass FROM pg_index WHERE NOT indisvalid;

CREATE INDEX CONCURRENTLY IF NOT EXISTS activities_asset_id_fk_idx ON portfolio.activities(asset_id) WHERE asset_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS activities_counter_account_id_fk_idx ON portfolio.activities(counter_account_id) WHERE counter_account_id IS NOT NULL;
