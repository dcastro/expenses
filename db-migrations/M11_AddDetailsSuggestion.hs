module M11_AddDetailsSuggestion where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Add a nullable `details_suggestion` column to `transaction_items`,
and add it to the `_transactions_with_items` view.

`NULL` means the item has no suggestion.

Ran on: 2026-11-02.
just run-migration 11 true
-}
migrate :: Connection -> IO ()
migrate conn = do
  SQL.execute_
    conn
    [sql|
      ALTER TABLE transaction_items ADD COLUMN details_suggestion TEXT
    |]

  SQL.execute_
    conn
    [sql|
      DROP VIEW _transactions_with_items
    |]

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
        ti.details_suggestion,
        ti.is_expense,
        ti.item_index,
        t.created_on AS transaction_created_on,
        ti.created_on AS item_created_on
      FROM transactions t
      LEFT JOIN transaction_items ti ON t.id = ti.transaction_id
      ORDER BY t.date DESC
    |]
