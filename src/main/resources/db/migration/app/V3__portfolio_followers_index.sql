-- Publishing a recommendation notifies every investor whose portfolio is backed by the model.
CREATE INDEX IF NOT EXISTS portfolios_backed_by_idx ON portfolios(backed_by) WHERE backed_by IS NOT NULL;
