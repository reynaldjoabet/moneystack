-- Budget module schema (PostgreSQL), applied by Flyway. Never edit once
-- deployed: add a new version. Idempotent; self-contained in the
-- `budget` schema. `owner_id` / `user_id` / `created_by` hold app user ids
-- without foreign keys. Amounts are minor units of the wallet's currency.

CREATE SCHEMA IF NOT EXISTS budget;

-- ---------- Wallets ----------

CREATE TABLE IF NOT EXISTS budget.wallets (
    id                     UUID PRIMARY KEY,
    owner_id               UUID         NOT NULL,
    name                   VARCHAR(200) NOT NULL,
    kind                   VARCHAR(16)  NOT NULL CHECK (kind IN ('cash','checking','savings','credit_card','loan','investment','other')),
    currency               CHAR(3)      NOT NULL,
    opening_balance_minor  BIGINT       NOT NULL DEFAULT 0,
    archived               BOOLEAN      NOT NULL DEFAULT false,
    created_at             TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS wallets_owner_idx ON budget.wallets(owner_id);

-- Read-only access for another member of the household.
CREATE TABLE IF NOT EXISTS budget.wallet_shares (
    wallet_id   UUID        NOT NULL REFERENCES budget.wallets(id) ON DELETE CASCADE,
    user_id     UUID        NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now(),
    PRIMARY KEY (wallet_id, user_id)
);

CREATE INDEX IF NOT EXISTS wallet_shares_user_idx ON budget.wallet_shares(user_id);

-- ---------- Categories, payees, tags ----------

CREATE TABLE IF NOT EXISTS budget.categories (
    id          UUID PRIMARY KEY,
    owner_id    UUID         NOT NULL,
    name        VARCHAR(200) NOT NULL,
    kind        VARCHAR(8)   NOT NULL CHECK (kind IN ('income','expense')),
    parent_id   UUID REFERENCES budget.categories(id) ON DELETE RESTRICT,
    colour      VARCHAR(16),
    archived    BOOLEAN      NOT NULL DEFAULT false,
    UNIQUE (owner_id, kind, name)
);

CREATE TABLE IF NOT EXISTS budget.payees (
    id                   UUID PRIMARY KEY,
    owner_id             UUID         NOT NULL,
    name                 VARCHAR(200) NOT NULL,
    aliases              TEXT[]       NOT NULL DEFAULT '{}',
    default_category_id  UUID REFERENCES budget.categories(id) ON DELETE SET NULL,
    UNIQUE (owner_id, name)
);

CREATE TABLE IF NOT EXISTS budget.tags (
    id        UUID PRIMARY KEY,
    owner_id  UUID         NOT NULL,
    name      VARCHAR(200) NOT NULL,
    UNIQUE (owner_id, name)
);

-- ---------- Planning ----------

CREATE TABLE IF NOT EXISTS budget.objectives (
    id            UUID PRIMARY KEY,
    owner_id      UUID         NOT NULL,
    kind          VARCHAR(8)   NOT NULL CHECK (kind IN ('goal','lent','borrowed')),
    name          VARCHAR(200) NOT NULL,
    target_minor  BIGINT       NOT NULL CHECK (target_minor > 0),
    currency      CHAR(3)      NOT NULL,
    due_date      DATE,
    counterparty  VARCHAR(200),
    archived      BOOLEAN      NOT NULL DEFAULT false,
    created_at    TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS budget.schedules (
    id                  UUID PRIMARY KEY,
    owner_id            UUID         NOT NULL,
    kind                VARCHAR(16)  NOT NULL CHECK (kind IN ('subscription','bill','installment','repetitive')),
    title               VARCHAR(200) NOT NULL,
    wallet_id           UUID         NOT NULL REFERENCES budget.wallets(id) ON DELETE CASCADE,
    tx_kind             VARCHAR(8)   NOT NULL CHECK (tx_kind IN ('income','expense')),
    amount_minor        BIGINT       NOT NULL CHECK (amount_minor > 0),
    category_id         UUID REFERENCES budget.categories(id) ON DELETE SET NULL,
    payee_id            UUID REFERENCES budget.payees(id) ON DELETE SET NULL,
    note                TEXT,
    frequency           VARCHAR(8)   NOT NULL CHECK (frequency IN ('daily','weekly','monthly','yearly')),
    every               INT          NOT NULL DEFAULT 1 CHECK (every BETWEEN 1 AND 366),
    start_date          DATE         NOT NULL,
    end_date            DATE,
    max_occurrences     INT CHECK (max_occurrences > 0),
    auto_pay            BOOLEAN      NOT NULL DEFAULT false,
    remind_days_before  INT          NOT NULL DEFAULT 3 CHECK (remind_days_before BETWEEN 0 AND 60),
    next_index          INT          NOT NULL DEFAULT 0 CHECK (next_index >= 0),
    active              BOOLEAN      NOT NULL DEFAULT true,
    created_at          TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CHECK (end_date IS NULL OR end_date >= start_date)
);

-- ---------- Transactions ----------

CREATE TABLE IF NOT EXISTS budget.transactions (
    id                UUID PRIMARY KEY,
    owner_id          UUID         NOT NULL,
    wallet_id         UUID         NOT NULL REFERENCES budget.wallets(id) ON DELETE CASCADE,
    kind              VARCHAR(8)   NOT NULL CHECK (kind IN ('income','expense','transfer')),
    amount_minor      BIGINT       NOT NULL CHECK (amount_minor > 0),
    tx_date           DATE         NOT NULL,
    title             VARCHAR(200) NOT NULL,
    note              TEXT,
    category_id       UUID REFERENCES budget.categories(id) ON DELETE SET NULL,
    category_source   VARCHAR(8)   NOT NULL DEFAULT 'unset' CHECK (category_source IN ('manual','payee','rule','schedule','unset')),
    payee_id          UUID REFERENCES budget.payees(id) ON DELETE SET NULL,
    tags              UUID[]       NOT NULL DEFAULT '{}',
    to_wallet_id      UUID REFERENCES budget.wallets(id) ON DELETE CASCADE,
    to_amount_minor   BIGINT CHECK (to_amount_minor > 0),
    refund_of         UUID REFERENCES budget.transactions(id) ON DELETE SET NULL,
    objective_id      UUID REFERENCES budget.objectives(id) ON DELETE SET NULL,
    schedule_id       UUID REFERENCES budget.schedules(id) ON DELETE SET NULL,
    due_date          DATE,
    excluded_budgets  UUID[]       NOT NULL DEFAULT '{}',
    import_key        VARCHAR(64),
    created_by        UUID         NOT NULL,
    created_at        TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CHECK ((kind = 'transfer') = (to_wallet_id IS NOT NULL)),
    CHECK (to_wallet_id IS NULL OR to_wallet_id <> wallet_id),
    CHECK ((schedule_id IS NULL) = (due_date IS NULL)),
    UNIQUE (schedule_id, due_date),
    UNIQUE (wallet_id, import_key)
);

CREATE INDEX IF NOT EXISTS transactions_wallet_idx ON budget.transactions(wallet_id, tx_date);
CREATE INDEX IF NOT EXISTS transactions_to_wallet_idx ON budget.transactions(to_wallet_id) WHERE to_wallet_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS budget.transaction_splits (
    transaction_id  UUID   NOT NULL REFERENCES budget.transactions(id) ON DELETE CASCADE,
    position        INT    NOT NULL,
    category_id     UUID   NOT NULL REFERENCES budget.categories(id) ON DELETE RESTRICT,
    amount_minor    BIGINT NOT NULL CHECK (amount_minor > 0),
    note            TEXT,
    PRIMARY KEY (transaction_id, position)
);

-- ---------- Budgets ----------

CREATE TABLE IF NOT EXISTS budget.budgets (
    id                     UUID PRIMARY KEY,
    owner_id               UUID         NOT NULL,
    name                   VARCHAR(200) NOT NULL,
    amount_minor           BIGINT       NOT NULL CHECK (amount_minor > 0),
    currency               CHAR(3)      NOT NULL,
    recurrence             VARCHAR(8)   NOT NULL CHECK (recurrence IN ('daily','weekly','monthly','yearly','custom')),
    period_length          INT          NOT NULL DEFAULT 1 CHECK (period_length BETWEEN 1 AND 366),
    start_date             DATE         NOT NULL,
    end_date               DATE,
    wallet_ids             UUID[]       NOT NULL DEFAULT '{}',
    category_ids           UUID[]       NOT NULL DEFAULT '{}',
    excluded_category_ids  UUID[]       NOT NULL DEFAULT '{}',
    income                 BOOLEAN      NOT NULL DEFAULT false,
    rollover               BOOLEAN      NOT NULL DEFAULT false,
    pinned                 BOOLEAN      NOT NULL DEFAULT false,
    archived               BOOLEAN      NOT NULL DEFAULT false,
    created_at             TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CHECK ((recurrence = 'custom') <= (end_date IS NOT NULL)),
    CHECK (end_date IS NULL OR end_date >= start_date)
);

CREATE TABLE IF NOT EXISTS budget.category_limits (
    budget_id     UUID   NOT NULL REFERENCES budget.budgets(id) ON DELETE CASCADE,
    category_id   UUID   NOT NULL REFERENCES budget.categories(id) ON DELETE CASCADE,
    amount_minor  BIGINT NOT NULL CHECK (amount_minor > 0),
    PRIMARY KEY (budget_id, category_id)
);

-- ---------- Automation rules ----------

CREATE TABLE IF NOT EXISTS budget.rules (
    id           UUID PRIMARY KEY,
    owner_id     UUID         NOT NULL,
    name         VARCHAR(200) NOT NULL,
    position     INT          NOT NULL,
    enabled      BOOLEAN      NOT NULL DEFAULT true,
    match_all    BOOLEAN      NOT NULL DEFAULT true,
    conditions   TEXT         NOT NULL,   -- JSON, see RuleJson
    actions      TEXT         NOT NULL,   -- JSON, see RuleJson
    match_count  BIGINT       NOT NULL DEFAULT 0,
    created_at   TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS rules_owner_idx ON budget.rules(owner_id, position);

-- ---------- Exchange rates ----------

CREATE TABLE IF NOT EXISTS budget.fx_rates (
    from_ccy   CHAR(3)         NOT NULL,
    to_ccy     CHAR(3)         NOT NULL,
    rate_date  DATE            NOT NULL,
    rate       NUMERIC(24, 10) NOT NULL CHECK (rate > 0),
    PRIMARY KEY (from_ccy, to_ccy, rate_date),
    CHECK (from_ccy <> to_ccy)
);
