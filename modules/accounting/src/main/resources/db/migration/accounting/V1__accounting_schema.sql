-- Accounting module schema (PostgreSQL), applied by Flyway. Never edit once
-- deployed: add a new version. Idempotent; self-contained (no
-- references to app tables). `created_by` / `updated_by` / `user_id` hold app
-- user ids without foreign keys so the module can be deployed on its own.

CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- ---------- Chart of accounts & rates ----------

CREATE TABLE IF NOT EXISTS ledger_accounts (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    code            VARCHAR(16)  NOT NULL UNIQUE,
    name            VARCHAR(200) NOT NULL,
    account_type    VARCHAR(32)  NOT NULL CHECK (account_type IN (
                        'cash','bank','accounts_receivable','inventory','other_current_asset','fixed_asset',
                        'non_current_asset','accounts_payable','credit_card','tax_payable','other_current_liability',
                        'long_term_liability','equity','income','other_income','cost_of_sales','expense','other_expense')),
    currency        CHAR(3)      NOT NULL,
    parent_id       UUID REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    active          BOOLEAN      NOT NULL DEFAULT true,
    created_at      TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- Units of base currency per one unit of `currency`, effective from `rate_date`.
CREATE TABLE IF NOT EXISTS exchange_rates (
    currency        CHAR(3)         NOT NULL,
    rate_date       DATE            NOT NULL,
    rate            NUMERIC(24, 10) NOT NULL CHECK (rate > 0),
    PRIMARY KEY (currency, rate_date)
);

-- ---------- Contacts ----------

CREATE TABLE IF NOT EXISTS contacts (
    id              UUID PRIMARY KEY,
    kind            VARCHAR(16)  NOT NULL CHECK (kind IN ('customer','vendor')),
    name            VARCHAR(200) NOT NULL,
    email           VARCHAR(254),
    currency        CHAR(3)      NOT NULL,
    user_id         UUID,
    active          BOOLEAN      NOT NULL DEFAULT true,
    created_at      TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX IF NOT EXISTS contacts_user_kind_idx ON contacts(user_id, kind) WHERE user_id IS NOT NULL;

-- ---------- Journal ----------

CREATE TABLE IF NOT EXISTS journal_entries (
    id              UUID PRIMARY KEY,
    entry_date      DATE            NOT NULL,
    reference       VARCHAR(64),
    memo            TEXT,
    currency        CHAR(3)         NOT NULL,
    exchange_rate   NUMERIC(24, 10) NOT NULL CHECK (exchange_rate > 0),
    source          VARCHAR(24)     NOT NULL CHECK (source IN (
                        'manual','reversal','payment','invoice','bill','receipt','bill_payment',
                        'fx_realized','inventory_issue','inventory_adjustment','bank_transaction',
                        'money_in','money_out','transfer')),
    source_id       UUID,
    created_by      UUID,
    created_at      TIMESTAMPTZ     NOT NULL DEFAULT now(),
    UNIQUE (source, source_id)   -- one posting per source document, one reversal per entry
);

CREATE INDEX IF NOT EXISTS journal_entries_date_idx ON journal_entries(entry_date, created_at);

-- Databases created before direct income/expense and transfers existed: widen the allowed sources.
ALTER TABLE journal_entries DROP CONSTRAINT IF EXISTS journal_entries_source_check;
ALTER TABLE journal_entries ADD CONSTRAINT journal_entries_source_check CHECK (source IN (
    'manual','reversal','payment','invoice','bill','receipt','bill_payment',
    'fx_realized','inventory_issue','inventory_adjustment','bank_transaction',
    'money_in','money_out','transfer'));

CREATE TABLE IF NOT EXISTS journal_lines (
    entry_id           UUID        NOT NULL REFERENCES journal_entries(id) ON DELETE RESTRICT,
    line_no            INT         NOT NULL CHECK (line_no > 0),
    account_id         UUID        NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    side               VARCHAR(6)  NOT NULL CHECK (side IN ('debit','credit')),
    amount_minor       BIGINT      NOT NULL CHECK (amount_minor > 0),   -- entry currency
    base_amount_minor  BIGINT      NOT NULL CHECK (base_amount_minor >= 0),
    contact_id         UUID REFERENCES contacts(id) ON DELETE RESTRICT,
    PRIMARY KEY (entry_id, line_no)
);

CREATE INDEX IF NOT EXISTS journal_lines_account_idx ON journal_lines(account_id);
CREATE INDEX IF NOT EXISTS journal_lines_contact_idx ON journal_lines(contact_id) WHERE contact_id IS NOT NULL;

-- Every entry must balance in base currency, checked once all its lines are in.
CREATE OR REPLACE FUNCTION journal_entry_balanced() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
    debit  BIGINT;
    credit BIGINT;
    n      INT;
BEGIN
    SELECT COALESCE(SUM(base_amount_minor) FILTER (WHERE side = 'debit'), 0),
           COALESCE(SUM(base_amount_minor) FILTER (WHERE side = 'credit'), 0),
           COUNT(*)
      INTO debit, credit, n
      FROM journal_lines WHERE entry_id = NEW.id;
    IF n < 2 OR debit <> credit THEN
        RAISE EXCEPTION 'journal entry % is unbalanced: debit %, credit %, % line(s)', NEW.id, debit, credit, n;
    END IF;
    RETURN NULL;
END $$;

DROP TRIGGER IF EXISTS journal_entries_balanced ON journal_entries;
CREATE CONSTRAINT TRIGGER journal_entries_balanced
    AFTER INSERT ON journal_entries
    DEFERRABLE INITIALLY DEFERRED
    FOR EACH ROW EXECUTE FUNCTION journal_entry_balanced();

-- Posted history is immutable; corrections go through reversing entries.
CREATE OR REPLACE FUNCTION append_only() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    RAISE EXCEPTION '% is append-only: % rejected', TG_TABLE_NAME, TG_OP;
END $$;

DROP TRIGGER IF EXISTS journal_entries_append_only ON journal_entries;
CREATE TRIGGER journal_entries_append_only
    BEFORE UPDATE OR DELETE ON journal_entries
    FOR EACH ROW EXECUTE FUNCTION append_only();

DROP TRIGGER IF EXISTS journal_lines_append_only ON journal_lines;
CREATE TRIGGER journal_lines_append_only
    BEFORE UPDATE OR DELETE ON journal_lines
    FOR EACH ROW EXECUTE FUNCTION append_only();

-- Single-row period lock. All-NULL columns mean the books are open; postings
-- take FOR SHARE on this row, so lock changes serialise against them.
CREATE TABLE IF NOT EXISTS ledger_lock (
    id              BOOLEAN PRIMARY KEY DEFAULT true CHECK (id),
    lock_through    DATE,
    unlock_from     DATE,
    unlock_to       DATE,
    reason          TEXT,
    updated_by      UUID,
    updated_at      TIMESTAMPTZ,
    CHECK ((unlock_from IS NULL) = (unlock_to IS NULL)),
    CHECK (unlock_from IS NULL OR unlock_from <= unlock_to)
);

INSERT INTO ledger_lock (id) VALUES (true) ON CONFLICT (id) DO NOTHING;

-- Gapless document numbers per series, allocated inside the posting transaction.
CREATE TABLE IF NOT EXISTS document_sequences (
    series          VARCHAR(16) PRIMARY KEY,
    next_value      BIGINT      NOT NULL DEFAULT 1 CHECK (next_value > 0)
);

INSERT INTO document_sequences (series) VALUES ('invoice'), ('bill'), ('receipt'), ('bill_payment')
    ON CONFLICT (series) DO NOTHING;

-- ---------- Tax rates ----------

CREATE TABLE IF NOT EXISTS tax_rates (
    id              UUID PRIMARY KEY,
    code            VARCHAR(16)  NOT NULL UNIQUE,
    name            VARCHAR(200) NOT NULL,
    rate_bps        INT          NOT NULL CHECK (rate_bps BETWEEN 0 AND 100000),
    active          BOOLEAN      NOT NULL DEFAULT true,
    created_at      TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- ---------- Items & inventory ----------

CREATE TABLE IF NOT EXISTS items (
    id                    UUID PRIMARY KEY,
    sku                   VARCHAR(32)  NOT NULL UNIQUE,
    name                  VARCHAR(200) NOT NULL,
    kind                  VARCHAR(16)  NOT NULL CHECK (kind IN ('service','non_inventory','inventory')),
    sell_price_minor      BIGINT CHECK (sell_price_minor > 0),
    cost_price_minor      BIGINT CHECK (cost_price_minor > 0),
    income_account_id     UUID NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    expense_account_id    UUID NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    inventory_account_id  UUID REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    active                BOOLEAN      NOT NULL DEFAULT true,
    created_at            TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CHECK ((kind = 'inventory') = (inventory_account_id IS NOT NULL))
);

-- Running position per inventory item (moving average cost, base currency).
CREATE TABLE IF NOT EXISTS item_stock (
    item_id         UUID PRIMARY KEY REFERENCES items(id) ON DELETE RESTRICT,
    quantity        NUMERIC(20, 6) NOT NULL DEFAULT 0 CHECK (quantity >= 0),
    value_minor     BIGINT         NOT NULL DEFAULT 0 CHECK (value_minor >= 0),
    CHECK (quantity > 0 OR value_minor = 0)
);

CREATE TABLE IF NOT EXISTS inventory_moves (
    id              BIGSERIAL PRIMARY KEY,
    item_id         UUID           NOT NULL REFERENCES items(id) ON DELETE RESTRICT,
    move_date       DATE           NOT NULL,
    quantity        NUMERIC(20, 6) NOT NULL CHECK (quantity <> 0),
    value_minor     BIGINT         NOT NULL,
    source          VARCHAR(24)    NOT NULL,
    source_id       UUID           NOT NULL,
    created_at      TIMESTAMPTZ    NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS inventory_moves_item_idx   ON inventory_moves(item_id, move_date);
CREATE INDEX IF NOT EXISTS inventory_moves_source_idx ON inventory_moves(source, source_id);

DROP TRIGGER IF EXISTS inventory_moves_append_only ON inventory_moves;
CREATE TRIGGER inventory_moves_append_only
    BEFORE UPDATE OR DELETE ON inventory_moves
    FOR EACH ROW EXECUTE FUNCTION append_only();

CREATE TABLE IF NOT EXISTS inventory_adjustments (
    id              UUID PRIMARY KEY,
    item_id         UUID           NOT NULL REFERENCES items(id) ON DELETE RESTRICT,
    adjust_date     DATE           NOT NULL,
    quantity        NUMERIC(20, 6) NOT NULL CHECK (quantity <> 0),
    value_minor     BIGINT         NOT NULL,
    account_id      UUID           NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    reason          TEXT           NOT NULL,
    entry_id        UUID           NOT NULL REFERENCES journal_entries(id) ON DELETE RESTRICT,
    created_by      UUID,
    created_at      TIMESTAMPTZ    NOT NULL DEFAULT now()
);

-- ---------- Invoices & bills ----------

CREATE TABLE IF NOT EXISTS documents (
    id                  UUID PRIMARY KEY,
    kind                VARCHAR(8)      NOT NULL CHECK (kind IN ('invoice','bill')),
    number              VARCHAR(32)     NOT NULL,
    contact_id          UUID            NOT NULL REFERENCES contacts(id) ON DELETE RESTRICT,
    issue_date          DATE            NOT NULL,
    due_date            DATE            NOT NULL CHECK (due_date >= issue_date),
    currency            CHAR(3)         NOT NULL,
    exchange_rate       NUMERIC(24, 10) NOT NULL CHECK (exchange_rate > 0),
    reference           VARCHAR(64),
    memo                TEXT,
    subtotal_minor      BIGINT          NOT NULL CHECK (subtotal_minor >= 0),
    tax_minor           BIGINT          NOT NULL CHECK (tax_minor >= 0),
    total_minor         BIGINT          NOT NULL CHECK (total_minor > 0 AND total_minor = subtotal_minor + tax_minor),
    control_base_minor  BIGINT          NOT NULL CHECK (control_base_minor >= 0),
    entry_id            UUID            NOT NULL REFERENCES journal_entries(id) ON DELETE RESTRICT,
    voided_on           DATE CHECK (voided_on >= issue_date),
    created_by          UUID,
    created_at          TIMESTAMPTZ     NOT NULL DEFAULT now(),
    UNIQUE (kind, number)
);

CREATE INDEX IF NOT EXISTS documents_contact_idx ON documents(kind, contact_id, issue_date);
CREATE INDEX IF NOT EXISTS documents_date_idx    ON documents(kind, issue_date);

CREATE TABLE IF NOT EXISTS document_lines (
    document_id       UUID           NOT NULL REFERENCES documents(id) ON DELETE RESTRICT,
    line_no           INT            NOT NULL CHECK (line_no > 0),
    item_id           UUID REFERENCES items(id) ON DELETE RESTRICT,
    description       TEXT,
    quantity          NUMERIC(20, 6) NOT NULL CHECK (quantity > 0),
    unit_price_minor  BIGINT         NOT NULL CHECK (unit_price_minor > 0),
    account_id        UUID           NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    tax_rate_id       UUID REFERENCES tax_rates(id) ON DELETE RESTRICT,
    tax_bps           INT CHECK (tax_bps BETWEEN 0 AND 100000),
    amount_minor      BIGINT         NOT NULL CHECK (amount_minor >= 0),
    tax_minor         BIGINT         NOT NULL CHECK (tax_minor >= 0),
    PRIMARY KEY (document_id, line_no),
    CHECK ((tax_rate_id IS NULL) = (tax_bps IS NULL))
);

-- ---------- Receipts & bill payments ----------

CREATE TABLE IF NOT EXISTS settlements (
    id               UUID PRIMARY KEY,
    kind             VARCHAR(8)      NOT NULL CHECK (kind IN ('invoice','bill')),
    number           VARCHAR(32)     NOT NULL,
    contact_id       UUID            NOT NULL REFERENCES contacts(id) ON DELETE RESTRICT,
    settle_date      DATE            NOT NULL,
    currency         CHAR(3)         NOT NULL,
    exchange_rate    NUMERIC(24, 10) NOT NULL CHECK (exchange_rate > 0),
    account_id       UUID            NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    reference        VARCHAR(64),
    memo             TEXT,
    total_minor      BIGINT          NOT NULL CHECK (total_minor > 0),
    entry_id         UUID            NOT NULL REFERENCES journal_entries(id) ON DELETE RESTRICT,
    fx_entry_id      UUID REFERENCES journal_entries(id) ON DELETE RESTRICT,
    voided_on        DATE CHECK (voided_on >= settle_date),
    created_by       UUID,
    created_at       TIMESTAMPTZ     NOT NULL DEFAULT now(),
    UNIQUE (kind, number)
);

CREATE TABLE IF NOT EXISTS settlement_allocations (
    settlement_id        UUID   NOT NULL REFERENCES settlements(id) ON DELETE RESTRICT,
    document_id          UUID   NOT NULL REFERENCES documents(id) ON DELETE RESTRICT,
    amount_minor         BIGINT NOT NULL CHECK (amount_minor > 0),
    base_minor           BIGINT NOT NULL,
    document_base_minor  BIGINT NOT NULL,
    PRIMARY KEY (settlement_id, document_id)
);

CREATE INDEX IF NOT EXISTS settlement_allocations_document_idx ON settlement_allocations(document_id);

-- ---------- Banking ----------

CREATE TABLE IF NOT EXISTS bank_rules (
    id               UUID PRIMARY KEY,
    name             VARCHAR(200) NOT NULL,
    priority         INT          NOT NULL DEFAULT 100,
    apply_to         VARCHAR(16)  NOT NULL CHECK (apply_to IN ('deposits','withdrawals','both')),
    conjunction      VARCHAR(8)   NOT NULL CHECK (conjunction IN ('all','any')),
    account_id       UUID         NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    contact_id       UUID REFERENCES contacts(id) ON DELETE RESTRICT,
    auto_categorize  BOOLEAN      NOT NULL DEFAULT false,
    active           BOOLEAN      NOT NULL DEFAULT true,
    created_at       TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS bank_rule_conditions (
    rule_id          UUID         NOT NULL REFERENCES bank_rules(id) ON DELETE CASCADE,
    position         INT          NOT NULL,
    field            VARCHAR(16)  NOT NULL CHECK (field IN ('description','amount')),
    operator         VARCHAR(16)  NOT NULL CHECK (operator IN ('contains','equals','starts_with','ends_with','greater_than','less_than')),
    value            VARCHAR(200) NOT NULL,
    PRIMARY KEY (rule_id, position)
);

CREATE TABLE IF NOT EXISTS bank_transactions (
    id                   UUID PRIMARY KEY,
    account_id           UUID         NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    tx_date              DATE         NOT NULL,
    description          VARCHAR(200) NOT NULL,
    amount_minor         BIGINT       NOT NULL CHECK (amount_minor <> 0),
    external_id          VARCHAR(128),
    status               VARCHAR(16)  NOT NULL CHECK (status IN ('uncategorized','categorized','excluded')),
    rule_id              UUID REFERENCES bank_rules(id) ON DELETE SET NULL,
    category_account_id  UUID REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    contact_id           UUID REFERENCES contacts(id) ON DELETE RESTRICT,
    entry_id             UUID REFERENCES journal_entries(id) ON DELETE RESTRICT,
    posting_id           UUID,
    created_at           TIMESTAMPTZ  NOT NULL DEFAULT now(),
    UNIQUE (account_id, external_id),
    CHECK ((status = 'categorized') = (entry_id IS NOT NULL))
);

CREATE INDEX IF NOT EXISTS bank_transactions_status_idx ON bank_transactions(account_id, status, tx_date);

-- ---------- Document workflow (drafts, history, recurring) ----------

-- Unposted, unnumbered invoices and bills; issuing one creates the document and deletes the draft.
CREATE TABLE IF NOT EXISTS document_drafts (
    id          UUID PRIMARY KEY,
    kind        VARCHAR(8)  NOT NULL CHECK (kind IN ('invoice','bill')),
    contact_id  UUID        NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    payload     TEXT        NOT NULL,
    created_by  UUID        NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS document_events (
    id           BIGSERIAL PRIMARY KEY,
    document_id  UUID        NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
    kind         VARCHAR(16) NOT NULL CHECK (kind IN ('issued','sent','payment','reminder','voided')),
    note         TEXT,
    by_user      UUID,
    at           TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS document_events_document_idx ON document_events(document_id, at);

CREATE TABLE IF NOT EXISTS recurring_documents (
    id               UUID PRIMARY KEY,
    kind             VARCHAR(8)  NOT NULL CHECK (kind IN ('invoice','bill')),
    contact_id       UUID        NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    payload          TEXT        NOT NULL,
    every            VARCHAR(8)  NOT NULL CHECK (every IN ('daily','weekly','monthly','yearly')),
    every_n          INT         NOT NULL DEFAULT 1 CHECK (every_n BETWEEN 1 AND 366),
    start_date       DATE        NOT NULL,
    end_date         DATE,
    max_occurrences  INT CHECK (max_occurrences > 0),
    next_index       INT         NOT NULL DEFAULT 0 CHECK (next_index >= 0),
    active           BOOLEAN     NOT NULL DEFAULT true,
    created_by       UUID        NOT NULL,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    CHECK (end_date IS NULL OR end_date >= start_date)
);

-- One document per occurrence, however often the scheduler runs.
CREATE TABLE IF NOT EXISTS recurring_runs (
    recurring_id  UUID NOT NULL REFERENCES recurring_documents(id) ON DELETE CASCADE,
    occurrence    INT  NOT NULL,
    document_id   UUID NOT NULL REFERENCES documents(id) ON DELETE RESTRICT,
    PRIMARY KEY (recurring_id, occurrence)
);

-- ---------- Bank reconciliation ----------

CREATE TABLE IF NOT EXISTS reconciliations (
    id             UUID PRIMARY KEY,
    account_id     UUID        NOT NULL REFERENCES ledger_accounts(id) ON DELETE RESTRICT,
    period_start   DATE        NOT NULL,
    period_end     DATE        NOT NULL CHECK (period_end >= period_start),
    opening_minor  BIGINT      NOT NULL,
    closing_minor  BIGINT      NOT NULL,
    status         VARCHAR(8)  NOT NULL CHECK (status IN ('open','closed')),
    created_by     UUID        NOT NULL,
    created_at     TIMESTAMPTZ NOT NULL DEFAULT now(),
    closed_at      TIMESTAMPTZ,
    CHECK ((status = 'closed') = (closed_at IS NOT NULL))
);

-- At most one open reconciliation per account.
CREATE UNIQUE INDEX IF NOT EXISTS reconciliations_open_idx ON reconciliations(account_id) WHERE status = 'open';

-- A ledger line is ticked off in at most one reconciliation.
CREATE TABLE IF NOT EXISTS reconciled_lines (
    reconciliation_id  UUID NOT NULL REFERENCES reconciliations(id) ON DELETE CASCADE,
    entry_id           UUID NOT NULL,
    line_no            INT  NOT NULL,
    PRIMARY KEY (entry_id, line_no),
    FOREIGN KEY (entry_id, line_no) REFERENCES journal_lines(entry_id, line_no) ON DELETE RESTRICT
);
