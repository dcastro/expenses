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
    UNIQUE (transaction_id, item_index)
  );

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
  ti.item_index,
  t.created_on AS transaction_created_on,
  ti.created_on AS item_created_on
FROM
  transactions t
  LEFT JOIN transaction_items ti ON t.id = ti.transaction_id
ORDER BY
  t.date DESC;
