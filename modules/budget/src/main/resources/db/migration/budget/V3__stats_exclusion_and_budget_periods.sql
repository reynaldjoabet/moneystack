-- A transaction can be left out of reports and budgets (a one-off such as selling a car) while still counting
-- toward balances. Adding a column with a constant default doesn't rewrite the table.
ALTER TABLE budget.transactions ADD COLUMN IF NOT EXISTS exclude_from_stats BOOLEAN NOT NULL DEFAULT false;

-- What a user set on one period of a budget: a note, and whether that period starts without the previous one's rollover.
CREATE TABLE IF NOT EXISTS budget.budget_periods (
    budget_id       UUID          NOT NULL REFERENCES budget.budgets(id) ON DELETE CASCADE,
    period_start    DATE          NOT NULL,
    note            VARCHAR(2000),
    rollover_reset  BOOLEAN       NOT NULL DEFAULT false,
    PRIMARY KEY (budget_id, period_start)
);
