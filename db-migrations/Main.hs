import CustomPrelude
import Database.SQLite.Simple qualified as SQL
import M01_AddItemCosts qualified
import M02_AddExpenseFlag qualified
import M03_AddTransactionsView qualified
import M04_NullableTags qualified
import M05_RenameTags qualified
import M06_RemoveQuotesFromIds qualified
import M07_RemoveBabyColumn qualified
import M08_MoveIsExpenseColumn qualified
import M09_AddSyncAccountStatus qualified
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (_target : dbPath : migrationId : _) ->
      SQL.withConnection dbPath \conn ->
        case migrationId of
          "1" -> do
            SQL.withTransaction conn $ M01_AddItemCosts.migrate conn
          "2" -> do
            SQL.withTransaction conn $ M02_AddExpenseFlag.migrate conn
          "3" -> do
            SQL.withTransaction conn $ M03_AddTransactionsView.migrate conn
          "4" -> do
            SQL.withTransaction conn $ M04_NullableTags.migrate conn
          "5" -> do
            SQL.withTransaction conn $ M05_RenameTags.migrate conn
          "6" -> do
            SQL.withTransaction conn $ M06_RemoveQuotesFromIds.migrate conn
          "7" -> do
            SQL.withTransaction conn $ M07_RemoveBabyColumn.migrate conn
          "8" -> do
            SQL.withTransaction conn $ M08_MoveIsExpenseColumn.migrate conn
          "9" -> do
            SQL.withTransaction conn $ M09_AddSyncAccountStatus.migrate conn
          _ -> do
            die $ "Invalid migration number: " <> show migrationId
    _ -> do
      die "Usage: db-migrations <db-path> <migration-id>"
