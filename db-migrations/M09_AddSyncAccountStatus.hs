module M09_AddSyncAccountStatus where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Add a table to persist the latest sync status per configured account.

Ran on 2026-05-30.
just run-migration 9 true
-}
migrate :: Connection -> IO ()
migrate conn = do
  SQL.execute_
    conn
    [sql|
      CREATE TABLE sync_account_status (
        account_id TEXT PRIMARY KEY,
        account_name TEXT NOT NULL,
        last_sync_finished_at DATETIME NOT NULL,
        last_sync_status TEXT NOT NULL,
        last_sync_error TEXT,
        last_synced_transaction_count INTEGER NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
      )
    |]

  SQL.execute_
    conn
    [sql|
      CREATE INDEX idx_sync_account_status_status
        ON sync_account_status (last_sync_status)
    |]
