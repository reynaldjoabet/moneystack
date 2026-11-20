-- Indexes on the referencing side of foreign keys into large tables. Postgres doesn't create these: without one,
-- every delete or key change in the parent scans the whole child table, holding its locks meanwhile.
-- Nullable keys get a partial index, which is smaller and still serves the foreign key check.
--
-- CONCURRENTLY builds each index without blocking writes, so it can't run in a transaction (see the .conf next to
-- this file). If a build fails, Postgres leaves an INVALID index that IF NOT EXISTS would then skip: drop it, run
-- `flyway repair`, and deploy again. Find them with
--   SELECT indexrelid::regclass FROM pg_index WHERE NOT indisvalid;

CREATE INDEX CONCURRENTLY IF NOT EXISTS bank_transactions_entry_id_fk_idx ON bank_transactions(entry_id) WHERE entry_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS bank_transactions_contact_id_fk_idx ON bank_transactions(contact_id) WHERE contact_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS bank_transactions_rule_id_fk_idx ON bank_transactions(rule_id) WHERE rule_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS bank_transactions_category_account_id_fk_idx ON bank_transactions(category_account_id) WHERE category_account_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS documents_contact_id_fk_idx ON documents(contact_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS documents_entry_id_fk_idx ON documents(entry_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS document_lines_account_id_fk_idx ON document_lines(account_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS document_lines_item_id_fk_idx ON document_lines(item_id) WHERE item_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS document_lines_tax_rate_id_fk_idx ON document_lines(tax_rate_id) WHERE tax_rate_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS settlements_contact_id_fk_idx ON settlements(contact_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS settlements_entry_id_fk_idx ON settlements(entry_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS settlements_fx_entry_id_fk_idx ON settlements(fx_entry_id) WHERE fx_entry_id IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS inventory_moves_item_id_fk_idx ON inventory_moves(item_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS reconciled_lines_reconciliation_id_fk_idx ON reconciled_lines(reconciliation_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS recurring_runs_document_id_fk_idx ON recurring_runs(document_id);
