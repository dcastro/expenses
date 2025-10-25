module M05_RenameTags where

import CustomPrelude
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

Data migration rename tags.

Ran on 2025-09-27.

```
Found 12 transaction items tagged as 'godaddy'
Found 20 transaction items tagged as 'software subs'
Found 41 transaction items tagged as 'ordem'
Found 21 transaction items tagged as 'lavagem'
Found 97 transaction items tagged as 'limpeza'
Found 10 transaction items tagged as 'repairs'
```

Migration task list:
* Disable the anacron job
* Backup gogle sheet sheet
* Advance "start from" in the config to the current month
* just run-migration 5 true
* Enable the anacron job
-}
migrate :: Connection -> IO ()
migrate conn = do
  let
    mapOldToNew :: [(Text, Text)]
    mapOldToNew =
      [ ("godaddy", "software")
      , ("software subs", "software")
      , ("ordem", "ordem dos enfermeiros")
      , ("lavagem", "car wash")
      , ("limpeza", "limpeza da casa")
      , ("repairs", "car repairs")
      ]

  forM_ mapOldToNew \(oldTag, newTag) -> do
    [Only @Int count] <-
      SQL.query
        conn
        [sql|SELECT COUNT(*) FROM transaction_items WHERE tag = ?|]
        (Only oldTag)

    putStrLn [i|Found #{count} transaction items tagged as '#{oldTag}'|]

    when (count > 0) do
      SQL.execute
        conn
        [sql|UPDATE transaction_items SET tag = ? WHERE tag = ?|]
        (newTag, oldTag)
