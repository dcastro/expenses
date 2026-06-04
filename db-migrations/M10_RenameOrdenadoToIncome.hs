module M10_RenameOrdenadoToIncome where

import CustomPrelude
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Rename the "ordenado" tag to "income".

Ran on 2026-06-04.
just run-migration 10 true
-}
migrate :: Connection -> IO ()
migrate conn = do
  [Only @Int count] <-
    SQL.query
      conn
      [sql|SELECT COUNT(*) FROM transaction_items WHERE tag = 'ordenado'|]
      ()

  putStrLn [i|Found #{count} transaction items tagged as 'ordenado'|]

  when (count > 0) do
    SQL.execute_
      conn
      [sql|UPDATE transaction_items SET tag = 'income' WHERE tag = 'ordenado'|]
