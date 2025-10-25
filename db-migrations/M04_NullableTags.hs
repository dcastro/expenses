module M04_NullableTags where

import CustomPrelude
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Data migration to mark missing tags as `NULL`.

So far, we've been using the string "!!!" to represent missing tags.
This was mainly because Google Sheets doesn't support NULL values.

Now we're trying to manage everythin in this application and will eventually be ditching Google Sheets,
we should start using NULL for this.


Ran on 2025-09-10.
There were 534 transaction items with the tag '!!!'.


Migration task list:
* Disable the anacron job
* Backup gogle sheet sheet
* Advance "start from" in the config to the current month
* just run-migration 4 true
* Enable the anacron job
-}
migrate :: Connection -> IO ()
migrate conn = do
  [Only @Int count] <- SQL.query_ conn [sql|SELECT COUNT(*) FROM transaction_items WHERE tag = '!!!'|]
  putStrLn [i|Found #{count} transaction items without a tag|]

  SQL.execute_
    conn
    [sql|UPDATE transaction_items SET tag = NULL WHERE tag = '!!!'|]
