-- Wealth management schema (PostgreSQL). Idempotent.

CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- ---------- Users / roles ----------

CREATE TABLE IF NOT EXISTS users (
    id                UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email             VARCHAR(254) NOT NULL UNIQUE,
    password_hash     VARCHAR(120) NOT NULL,
    display_name      VARCHAR(120) NOT NULL,
    role              VARCHAR(16)  NOT NULL CHECK (role IN ('investor','analyst','accountant','admin')),
    locale            VARCHAR(2)   NOT NULL,
    telegram_chat_id  BIGINT,
    created_at        TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- ---------- Instruments reference ----------

CREATE TABLE IF NOT EXISTS instruments (
    ticker       VARCHAR(10)  NOT NULL,
    exchange     VARCHAR(16)  NOT NULL,
    isin         CHAR(12),
    kind         VARCHAR(16)  NOT NULL,
    name         VARCHAR(200) NOT NULL,
    currency     CHAR(3)      NOT NULL,
    PRIMARY KEY (ticker, exchange)
);

-- ---------- Model portfolios ----------

CREATE TABLE IF NOT EXISTS model_portfolios (
    id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name         VARCHAR(200) NOT NULL,
    strategy     VARCHAR(24)  NOT NULL,
    created_by   UUID         NOT NULL REFERENCES users(id) ON DELETE RESTRICT,
    description  TEXT,
    created_at   TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at   TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS model_allocations (
    model_portfolio_id UUID         NOT NULL REFERENCES model_portfolios(id) ON DELETE CASCADE,
    ticker             VARCHAR(10)  NOT NULL,
    exchange           VARCHAR(16)  NOT NULL,
    weight_bps         INT          NOT NULL CHECK (weight_bps BETWEEN 0 AND 10000),
    PRIMARY KEY (model_portfolio_id, ticker, exchange)
);

-- ---------- Investor portfolios + holdings ----------

CREATE TABLE IF NOT EXISTS portfolios (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id         UUID         NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name            VARCHAR(200) NOT NULL,
    base_currency   CHAR(3)      NOT NULL,
    backed_by       UUID REFERENCES model_portfolios(id) ON DELETE SET NULL,
    created_at      TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS portfolios_user_idx ON portfolios(user_id);

CREATE TABLE IF NOT EXISTS holdings (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    portfolio_id    UUID            NOT NULL REFERENCES portfolios(id) ON DELETE CASCADE,
    ticker          VARCHAR(10)     NOT NULL,
    exchange        VARCHAR(16)     NOT NULL,
    quantity        NUMERIC(38, 18) NOT NULL CHECK (quantity > 0),
    avg_cost_unit   NUMERIC(38, 18) NOT NULL CHECK (avg_cost_unit > 0),
    updated_at      TIMESTAMPTZ     NOT NULL DEFAULT now(),
    UNIQUE (portfolio_id, ticker, exchange)
);

CREATE INDEX IF NOT EXISTS holdings_portfolio_idx ON holdings(portfolio_id);

-- ---------- Recommendations & news ----------

CREATE TABLE IF NOT EXISTS recommendations (
    id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    model_portfolio_id   UUID         NOT NULL REFERENCES model_portfolios(id) ON DELETE CASCADE,
    ticker               VARCHAR(10)  NOT NULL,
    exchange             VARCHAR(16)  NOT NULL,
    action               VARCHAR(8)   NOT NULL CHECK (action IN ('buy','sell','hold')),
    rationale            TEXT         NOT NULL,
    target_weight_bps    INT CHECK (target_weight_bps BETWEEN 0 AND 10000),
    published_at         TIMESTAMPTZ  NOT NULL DEFAULT now(),
    published_by         UUID         NOT NULL REFERENCES users(id) ON DELETE RESTRICT
);

CREATE INDEX IF NOT EXISTS recommendations_model_idx ON recommendations(model_portfolio_id, published_at DESC);

CREATE TABLE IF NOT EXISTS news_items (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    title           VARCHAR(200) NOT NULL,
    body            TEXT         NOT NULL,
    tickers         VARCHAR(10)[] NOT NULL DEFAULT ARRAY[]::VARCHAR(10)[],
    published_at    TIMESTAMPTZ  NOT NULL DEFAULT now(),
    author_id       UUID REFERENCES users(id) ON DELETE SET NULL,
    source          VARCHAR(64)  NOT NULL
);

CREATE INDEX IF NOT EXISTS news_published_idx ON news_items(published_at DESC);
CREATE INDEX IF NOT EXISTS news_tickers_gin   ON news_items USING GIN (tickers);

CREATE TABLE IF NOT EXISTS alerts (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id         UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    kind            VARCHAR(32) NOT NULL,
    payload         JSONB       NOT NULL,
    delivered_at    TIMESTAMPTZ
);

-- ---------- Subscriptions & payments ----------

CREATE TABLE IF NOT EXISTS plans (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    code            VARCHAR(32)  NOT NULL UNIQUE,
    name            VARCHAR(200) NOT NULL,
    price_minor     BIGINT       NOT NULL CHECK (price_minor > 0),
    currency        CHAR(3)      NOT NULL,
    interval        VARCHAR(16)  NOT NULL CHECK (interval IN ('monthly','annual')),
    active          BOOLEAN      NOT NULL DEFAULT true
);

CREATE TABLE IF NOT EXISTS subscriptions (
    id                    UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id               UUID         NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    plan_id               UUID         NOT NULL REFERENCES plans(id) ON DELETE RESTRICT,
    status                VARCHAR(16)  NOT NULL,
    current_period_start  TIMESTAMPTZ  NOT NULL,
    current_period_end    TIMESTAMPTZ  NOT NULL,
    cancel_at             TIMESTAMPTZ,
    cloudpayments_ref     VARCHAR(128)
);

CREATE INDEX IF NOT EXISTS subscriptions_user_idx ON subscriptions(user_id);

CREATE TABLE IF NOT EXISTS payments (
    id                  UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id             UUID         NOT NULL REFERENCES users(id) ON DELETE RESTRICT,
    subscription_id     UUID REFERENCES subscriptions(id) ON DELETE SET NULL,
    course_id           UUID,
    amount_minor        BIGINT       NOT NULL CHECK (amount_minor > 0),
    currency            CHAR(3)      NOT NULL,
    status              VARCHAR(16)  NOT NULL,
    cloudpayments_ref   VARCHAR(128) NOT NULL,
    created_at          TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS payments_user_idx ON payments(user_id, created_at DESC);

-- ---------- E-learning ----------

CREATE TABLE IF NOT EXISTS courses (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    title           VARCHAR(200) NOT NULL,
    description     TEXT         NOT NULL,
    price_minor     BIGINT       NOT NULL CHECK (price_minor > 0),
    currency        CHAR(3)      NOT NULL,
    minutes         INT          NOT NULL CHECK (minutes > 0),
    published_at    TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS webinars (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    title           VARCHAR(200) NOT NULL,
    description     TEXT         NOT NULL,
    starts_at       TIMESTAMPTZ  NOT NULL,
    ends_at         TIMESTAMPTZ  NOT NULL,
    stream_url      VARCHAR(512) NOT NULL,
    host_id         UUID         NOT NULL REFERENCES users(id) ON DELETE RESTRICT
);

CREATE TABLE IF NOT EXISTS enrollments (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id         UUID         NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    course_id       UUID REFERENCES courses(id) ON DELETE CASCADE,
    webinar_id      UUID REFERENCES webinars(id) ON DELETE CASCADE,
    enrolled_at     TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CHECK ((course_id IS NULL) <> (webinar_id IS NULL))
);

CREATE INDEX IF NOT EXISTS enrollments_user_idx ON enrollments(user_id);

-- Accounting tables live in modules/accounting/src/main/resources/db/accounting.sql;
-- apply it after this file.
