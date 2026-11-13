-- Security events: who did what to which account, and from where. Rows are only ever added.
CREATE TABLE IF NOT EXISTS audit_events (
    id             BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    at             TIMESTAMPTZ  NOT NULL DEFAULT now(),
    action         VARCHAR(40)  NOT NULL,
    -- Not foreign keys: the record must outlive the accounts it mentions.
    actor_id       UUID,
    subject_id     UUID,
    request_id     VARCHAR(64),
    forwarded_for  VARCHAR(200),
    detail         VARCHAR(200)
);

CREATE INDEX IF NOT EXISTS audit_events_subject_idx ON audit_events(subject_id, id DESC);
