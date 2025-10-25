module M08_MoveIsExpenseColumn where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Moved `is_expense` column from `transactions` table to `transaction_items` table.

Ran on 2025-10-21.
just run-migration 7 true
-}
migrate :: Connection -> IO ()
migrate conn = do
  -- 1. Drop the view to allow table changes
  SQL.execute_ conn [sql|DROP VIEW _transactions_with_items|]

  -- 2. Add is_expense column to transaction_items
  SQL.execute_ conn [sql|ALTER TABLE transaction_items ADD COLUMN is_expense BOOLEAN|]

  -- 3. Copy is_expense values from transactions to transaction_items
  SQL.execute_
    conn
    [sql|
    UPDATE transaction_items
    SET is_expense = (
      SELECT is_expense FROM transactions WHERE transactions.id = transaction_items.transaction_id
    )
  |]

  -- 4. Remove is_expense column from transactions
  SQL.execute_ conn [sql|ALTER TABLE transactions DROP COLUMN is_expense|]

  -- 5. Recreate the view with is_expense from transaction_items
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
        ti.item_index,
        t.created_on AS transaction_created_on,
        ti.created_on AS item_created_on
      FROM transactions t
      LEFT JOIN transaction_items ti
        ON t.id = ti.transaction_id
      ORDER BY t.date DESC;
    |]
