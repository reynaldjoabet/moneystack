-- The most of income a tag's spending should take, shown against the tag analysis.
ALTER TABLE budget.tags
    ADD COLUMN IF NOT EXISTS target_pct NUMERIC(5, 2) CHECK (target_pct > 0 AND target_pct <= 100);
