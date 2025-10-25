module M03_AddTransactionsView where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Ran on 24/07/2025.

Migration task list:
* Disable the anacron job
* Backup gogle sheet sheet
* Backup database
* Advance "start from" in the config to the current month
* stack run expenses:exe:db-migrations -- 2
* Enable the anacron job
-}
migrate :: Connection -> IO ()
migrate conn = do
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
        ti.baby,
        t.created_on AS transaction_created_on,
        ti.created_on AS item_created_on
      FROM transactions t
      LEFT JOIN transaction_items ti
        ON t.id = ti.transaction_id
      ORDER BY t.date DESC;
    |]
