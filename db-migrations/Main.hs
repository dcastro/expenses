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
import M10_RenameOrdenadoToIncome qualified
import M11_AddBudgetConfig qualified
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (dbPath : migrationId : _) ->
      SQL.withConnection dbPath \conn -> do
        -- Run every migration in a transaction
        SQL.withTransaction conn do
          case migrationId of
            "1" -> M01_AddItemCosts.migrate conn
            "2" -> M02_AddExpenseFlag.migrate conn
            "3" -> M03_AddTransactionsView.migrate conn
            "4" -> M04_NullableTags.migrate conn
            "5" -> M05_RenameTags.migrate conn
            "6" -> M06_RemoveQuotesFromIds.migrate conn
            "7" -> M07_RemoveBabyColumn.migrate conn
            "8" -> M08_MoveIsExpenseColumn.migrate conn
            "9" -> M09_AddSyncAccountStatus.migrate conn
            "10" -> M10_RenameOrdenadoToIncome.migrate conn
            "11" -> M11_AddBudgetConfig.migrate conn
            _ -> do
              die $ "Invalid migration number: " <> show migrationId
    _ -> do
      die "Usage: db-migrations <db-path> <migration-id>"
