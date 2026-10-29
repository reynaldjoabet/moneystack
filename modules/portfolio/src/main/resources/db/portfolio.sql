-- Portfolio module schema (PostgreSQL). Idempotent; self-contained in the
-- `portfolio` schema. `owner_id` holds app user ids without foreign keys.

CREATE SCHEMA IF NOT EXISTS portfolio;

-- ---------- Accounts ----------

CREATE TABLE IF NOT EXISTS portfolio.accounts (
    id              UUID PRIMARY KEY,
    owner_id        UUID         NOT NULL,
    name            VARCHAR(200) NOT NULL,
    kind            VARCHAR(16)  NOT NULL CHECK (kind IN ('brokerage','retirement','tax_advantaged','crypto','savings','cash','other')),
    tracking        VARCHAR(12)  NOT NULL CHECK (tracking IN ('activities','balances')),
    currency        CHAR(3)      NOT NULL,
    account_group   VARCHAR(200),
    active          BOOLEAN      NOT NULL DEFAULT true,
    created_at      TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS accounts_owner_idx ON portfolio.accounts(owner_id);

CREATE TABLE IF NOT EXISTS portfolio.balance_snapshots (
    account_id      UUID           NOT NULL REFERENCES portfolio.accounts(id) ON DELETE CASCADE,
    snapshot_date   DATE           NOT NULL,
    balance         NUMERIC(20, 6) NOT NULL,
    invested        NUMERIC(20, 6) NOT NULL DEFAULT 0,
    PRIMARY KEY (account_id, snapshot_date)
);

-- ---------- Assets & market data (shared across owners) ----------

CREATE TABLE IF NOT EXISTS portfolio.assets (
    id              UUID PRIMARY KEY,
    symbol          VARCHAR(32)  NOT NULL UNIQUE,
    name            VARCHAR(200) NOT NULL,
    kind            VARCHAR(16)  NOT NULL CHECK (kind IN ('equity','etf','fund','bond','crypto','commodity','cash','other')),
    currency        CHAR(3)      NOT NULL,
    created_at      TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS portfolio.classifications (
    asset_id        UUID         NOT NULL REFERENCES portfolio.assets(id) ON DELETE CASCADE,
    taxonomy        VARCHAR(32)  NOT NULL,
    category        VARCHAR(200) NOT NULL,
    weight_bps      INT          NOT NULL CHECK (weight_bps BETWEEN 1 AND 10000),
    PRIMARY KEY (asset_id, taxonomy, category)
);

CREATE TABLE IF NOT EXISTS portfolio.quotes (
    asset_id        UUID            NOT NULL REFERENCES portfolio.assets(id) ON DELETE CASCADE,
    quote_date      DATE            NOT NULL,
    close           NUMERIC(24, 10) NOT NULL CHECK (close >= 0),
    PRIMARY KEY (asset_id, quote_date)
);

CREATE TABLE IF NOT EXISTS portfolio.fx_rates (
    from_ccy        CHAR(3)         NOT NULL,
    to_ccy          CHAR(3)         NOT NULL,
    rate_date       DATE            NOT NULL,
    rate            NUMERIC(24, 10) NOT NULL CHECK (rate > 0),
    PRIMARY KEY (from_ccy, to_ccy, rate_date),
    CHECK (from_ccy <> to_ccy)
);

-- ---------- Activities ----------

CREATE TABLE IF NOT EXISTS portfolio.activities (
    id                  UUID PRIMARY KEY,
    account_id          UUID            NOT NULL REFERENCES portfolio.accounts(id) ON DELETE CASCADE,
    asset_id            UUID REFERENCES portfolio.assets(id) ON DELETE RESTRICT,
    kind                VARCHAR(16)     NOT NULL CHECK (kind IN ('buy','sell','dividend','interest','deposit','withdrawal',
                                          'transfer_in','transfer_out','fee','tax','split','credit','adjustment')),
    activity_date       DATE            NOT NULL,
    quantity            NUMERIC(30, 10),
    unit_price          NUMERIC(24, 10),
    amount              NUMERIC(20, 6),
    fee                 NUMERIC(20, 6)  NOT NULL DEFAULT 0 CHECK (fee >= 0),
    currency            CHAR(3)         NOT NULL,
    external            BOOLEAN         NOT NULL DEFAULT false,
    counter_account_id  UUID REFERENCES portfolio.accounts(id) ON DELETE SET NULL,
    note                TEXT,
    import_key          VARCHAR(64),
    created_at          TIMESTAMPTZ     NOT NULL DEFAULT now(),
    UNIQUE (account_id, import_key)
);

CREATE INDEX IF NOT EXISTS activities_account_idx ON portfolio.activities(account_id, activity_date);

-- ---------- Planning ----------

CREATE TABLE IF NOT EXISTS portfolio.allocation_targets (
    id              UUID PRIMARY KEY,
    owner_id        UUID         NOT NULL,
    taxonomy        VARCHAR(32)  NOT NULL,
    category        VARCHAR(200) NOT NULL,
    weight_bps      INT          NOT NULL CHECK (weight_bps BETWEEN 0 AND 10000),
    UNIQUE (owner_id, taxonomy, category)
);

CREATE TABLE IF NOT EXISTS portfolio.contribution_limits (
    id              UUID PRIMARY KEY,
    owner_id        UUID           NOT NULL,
    name            VARCHAR(200)   NOT NULL,
    contribution_year INT          NOT NULL CHECK (contribution_year BETWEEN 1900 AND 2200),
    limit_amount    NUMERIC(20, 6) NOT NULL CHECK (limit_amount > 0),
    currency        CHAR(3)        NOT NULL,
    start_date      DATE,
    end_date        DATE,
    CHECK (start_date IS NULL OR end_date IS NULL OR start_date <= end_date)
);

CREATE TABLE IF NOT EXISTS portfolio.contribution_limit_accounts (
    limit_id        UUID NOT NULL REFERENCES portfolio.contribution_limits(id) ON DELETE CASCADE,
    account_id      UUID NOT NULL REFERENCES portfolio.accounts(id) ON DELETE CASCADE,
    PRIMARY KEY (limit_id, account_id)
);

CREATE TABLE IF NOT EXISTS portfolio.goals (
    id                    UUID PRIMARY KEY,
    owner_id              UUID           NOT NULL,
    name                  VARCHAR(200)   NOT NULL,
    target_amount         NUMERIC(20, 6) NOT NULL CHECK (target_amount > 0),
    currency              CHAR(3)        NOT NULL,
    target_date           DATE,
    monthly_contribution  NUMERIC(20, 6) NOT NULL DEFAULT 0 CHECK (monthly_contribution >= 0),
    expected_return       NUMERIC(24, 10) NOT NULL DEFAULT 0,
    created_at            TIMESTAMPTZ    NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS portfolio.goal_funding (
    goal_id         UUID NOT NULL REFERENCES portfolio.goals(id) ON DELETE CASCADE,
    account_id      UUID NOT NULL REFERENCES portfolio.accounts(id) ON DELETE CASCADE,
    share_bps       INT  NOT NULL CHECK (share_bps BETWEEN 1 AND 10000),
    PRIMARY KEY (goal_id, account_id)
);

-- ---------- Net worth: alternative assets & liabilities ----------

CREATE TABLE IF NOT EXISTS portfolio.alt_assets (
    id                UUID PRIMARY KEY,
    owner_id          UUID           NOT NULL,
    kind              VARCHAR(16)    NOT NULL CHECK (kind IN ('real_estate','vehicle','collectible','business','other')),
    name              VARCHAR(200)   NOT NULL,
    currency          CHAR(3)        NOT NULL,
    purchase_price    NUMERIC(20, 6) NOT NULL CHECK (purchase_price >= 0),
    purchase_date     DATE           NOT NULL,
    ownership_bps     INT            NOT NULL DEFAULT 10000 CHECK (ownership_bps BETWEEN 1 AND 10000),
    acquisition_costs NUMERIC(20, 6) NOT NULL DEFAULT 0 CHECK (acquisition_costs >= 0),
    vehicle_class     VARCHAR(16),
    pace              VARCHAR(8),
    notes             TEXT,
    active            BOOLEAN        NOT NULL DEFAULT true,
    created_at        TIMESTAMPTZ    NOT NULL DEFAULT now(),
    CHECK ((kind = 'vehicle') = (vehicle_class IS NOT NULL))
);

CREATE TABLE IF NOT EXISTS portfolio.alt_valuations (
    asset_id        UUID           NOT NULL REFERENCES portfolio.alt_assets(id) ON DELETE CASCADE,
    valuation_date  DATE           NOT NULL,
    value           NUMERIC(20, 6) NOT NULL CHECK (value >= 0),
    source          VARCHAR(200)   NOT NULL,
    PRIMARY KEY (asset_id, valuation_date)
);

CREATE TABLE IF NOT EXISTS portfolio.liabilities (
    id                    UUID PRIMARY KEY,
    owner_id              UUID            NOT NULL,
    kind                  VARCHAR(16)     NOT NULL CHECK (kind IN ('mortgage','auto','student','personal','heloc','business','medical','credit_card','other')),
    name                  VARCHAR(200)    NOT NULL,
    currency              CHAR(3)         NOT NULL,
    principal             NUMERIC(20, 6)  NOT NULL CHECK (principal > 0),
    annual_rate           NUMERIC(24, 10) NOT NULL CHECK (annual_rate >= 0 AND annual_rate < 1),
    term_months           INT             NOT NULL CHECK (term_months BETWEEN 1 AND 600),
    start_date            DATE            NOT NULL,
    extra_monthly_payment NUMERIC(20, 6)  NOT NULL DEFAULT 0 CHECK (extra_monthly_payment >= 0),
    secured_by            UUID REFERENCES portfolio.alt_assets(id) ON DELETE SET NULL,
    active                BOOLEAN         NOT NULL DEFAULT true,
    created_at            TIMESTAMPTZ     NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS portfolio.liability_statements (
    liability_id    UUID           NOT NULL REFERENCES portfolio.liabilities(id) ON DELETE CASCADE,
    statement_date  DATE           NOT NULL,
    balance         NUMERIC(20, 6) NOT NULL CHECK (balance >= 0),
    PRIMARY KEY (liability_id, statement_date)
);
