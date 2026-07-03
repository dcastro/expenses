CREATE TABLE
  transactions (
    id TEXT PRIMARY KEY,
    account TEXT,
    date TEXT,
    desc TEXT,
    total_amount_cents INTEGER,
    created_on DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
  );

CREATE TABLE
  transaction_items (
    transaction_id TEXT NOT NULL REFERENCES transactions (id),
    item_index INTEGER NOT NULL,
    item_amount_cents INTEGER NOT NULL,
    tag TEXT,
    details TEXT,
    created_on DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
    is_expense BOOLEAN NOT NULL,
    budget_override BOOLEAN,
    UNIQUE (transaction_id, item_index)
  );

CREATE TABLE
  sync_account_status (
    account_id TEXT PRIMARY KEY,
    account_name TEXT NOT NULL,
    last_sync_finished_at DATETIME NOT NULL,
    last_sync_status TEXT NOT NULL,
    last_sync_error TEXT,
    last_synced_transaction_count INTEGER NOT NULL,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
  );

CREATE INDEX
  idx_sync_account_status_status ON sync_account_status (last_sync_status);

CREATE VIEW
  _transactions_with_items AS
SELECT
  t.id AS transaction_id,
  t.account,
  t.date,
  t.desc,
  CAST(t.total_amount_cents as REAL) / 100 as total_amount,
  CAST(ti.item_amount_cents as REAL) / 100 as item_amount,
  ti.tag,
  ti.details,
  ti.is_expense,
  ti.budget_override,
  ti.item_index,
  t.created_on AS transaction_created_on,
  ti.created_on AS item_created_on
FROM
  transactions t
  LEFT JOIN transaction_items ti ON t.id = ti.transaction_id
ORDER BY
  t.date DESC;
