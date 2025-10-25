module M07_RemoveBabyColumn where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Dropped column 'baby' from 'transaction_items' table.

Ran on 2025-10-20.
just run-migration 7 true
-}
migrate :: Connection -> IO ()
migrate conn = do
  SQL.execute_ conn [sql|DROP VIEW _transactions_with_items|]
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
        t.is_expense,
        ti.item_index,
        t.created_on AS transaction_created_on,
        ti.created_on AS item_created_on
      FROM transactions t
      LEFT JOIN transaction_items ti
        ON t.id = ti.transaction_id
      ORDER BY t.date DESC;
    |]
  SQL.execute_ conn [sql|ALTER TABLE transaction_items DROP COLUMN baby|]
