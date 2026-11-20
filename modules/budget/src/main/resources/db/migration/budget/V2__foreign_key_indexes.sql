-- Indexes on the referencing side of foreign keys into large tables. Postgres doesn't create these: without one,
-- every delete or key change in the parent scans the whole child table, holding its locks meanwhile.
-- Nullable keys get a partial index, which is smaller and still serves the foreign key check.
--
-- CONCURRENTLY builds each index without blocking writes, so it can't run in a transaction (see the .conf next to
-- this file). If a build fails, Postgres leaves an INVALID index that IF NOT EXISTS would then skip: drop it, run
-- `flyway repair`, and deploy again. Find them with
--   SELECT indexrelid::regclass FROM pg_index WHERE NOT indisvalid;

CREATE INDEX CONCURRENTLY IF NOT EXISTS transactions_category_id_fk_idx ON budget.transactions(category_id) WHERE category_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS transactions_payee_id_fk_idx ON budget.transactions(payee_id) WHERE payee_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS transactions_objective_id_fk_idx ON budget.transactions(objective_id) WHERE objective_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS transactions_refund_of_fk_idx ON budget.transactions(refund_of) WHERE refund_of IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS transaction_splits_category_id_fk_idx ON budget.transaction_splits(category_id);
