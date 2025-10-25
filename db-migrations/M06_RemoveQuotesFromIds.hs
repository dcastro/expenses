module M06_RemoveQuotesFromIds where

import CustomPrelude
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

This migration removes double quotes from IDs in the transactions table.
For example, an ID '"abc123"' will become 'abc123'.


Ran on 2025-10-19.
just run-migration 6 true
Found 5336 transaction IDs with surrounding quotes
-}
migrate :: Connection -> IO ()
migrate conn = do
  -- Find all IDs surrounded by double quotes
  idsWithQuotes <- SQL.query_ conn [sql|SELECT id FROM transactions WHERE id LIKE '"%"'|] :: IO [Only Text]
  putStrLn [i|Found #{length idsWithQuotes} transaction IDs with surrounding quotes|]

  forM_ idsWithQuotes $ \(Only quotedId) -> do
    let newId = stripQuotes quotedId
    -- Update the ID in transactions
    SQL.execute conn [sql|UPDATE transactions SET id = ? WHERE id = ?|] (newId, quotedId)
    -- Update any references in transaction_items
    SQL.execute conn [sql|UPDATE transaction_items SET transaction_id = ? WHERE transaction_id = ?|] (newId, quotedId)
    putStrLn [i|Updated ID '#{quotedId}' to '#{newId}'|]

-- >>> stripQuotes "\"abc123\""
-- "abc123"
--
-- >>> stripQuotes "abc123"
-- "abc123"
stripQuotes :: Text -> Text
stripQuotes =
  T.dropAround (== '"')
