module M11_AddBudgetOverride where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Add a nullable `budget_override` column to `transaction_items`.

This lets the user override whether a transaction item is considered part of
the budget:
  * NULL  -> use the default matching rules (account AND tag)
  * TRUE  -> always include in the budget
  * FALSE -> always exclude from the budget

Ran on TODO.
just run-migration 11 true
-}
migrate :: Connection -> IO ()
migrate conn = do
  -- Drop the view so we can recreate it with the new column.
  SQL.execute_ conn [sql|DROP VIEW _transactions_with_items|]

  -- Nullable, defaults to NULL (no override).
  SQL.execute_ conn [sql|ALTER TABLE transaction_items ADD COLUMN budget_override BOOLEAN|]

  SQL.execute_
    conn
    [sql|
      CREATE VIEW _transactions_with_items AS
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
      FROM transactions t
      LEFT JOIN transaction_items ti
        ON t.id = ti.transaction_id
      ORDER BY t.date DESC;
    |]
