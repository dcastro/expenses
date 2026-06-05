module M11_AddBudgetConfig where

import CustomPrelude
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Create the app_settings table with a single row holding the monthly budget limit.
Stored as BECents (negative for expenses). Default limit: -120000 cents (1200 EUR).

-}
migrate :: Connection -> IO ()
migrate conn = do
  SQL.execute_ conn [sql|
    CREATE TABLE app_settings (
      id INTEGER PRIMARY KEY CHECK (id = 1),
      monthly_budget_limit_cents INTEGER NOT NULL DEFAULT -120000
    )
  |]
  SQL.execute_ conn [sql|
    INSERT INTO app_settings (id, monthly_budget_limit_cents) VALUES (1, -120000)
  |]
  putStrLn [i|Created app_settings table with default monthly_budget_limit_cents = -120000|]
