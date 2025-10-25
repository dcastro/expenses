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
import System.Environment (getArgs)
import Util qualified

main :: IO ()
main = do
  args <- getArgs
  Util.withConnection "/home/dc/.local/share/expenses-manager/expenses.db" \conn ->
    case drop 1 args of
      ("1" : _) -> do
        SQL.withTransaction conn $ M01_AddItemCosts.migrate conn
      ("2" : _) -> do
        SQL.withTransaction conn $ M02_AddExpenseFlag.migrate conn
      ("3" : _) -> do
        SQL.withTransaction conn $ M03_AddTransactionsView.migrate conn
      ("4" : _) -> do
        SQL.withTransaction conn $ M04_NullableTags.migrate conn
      ("5" : _) -> do
        SQL.withTransaction conn $ M05_RenameTags.migrate conn
      ("6" : _) -> do
        SQL.withTransaction conn $ M06_RemoveQuotesFromIds.migrate conn
      ("7" : _) -> do
        SQL.withTransaction conn $ M07_RemoveBabyColumn.migrate conn
      ("8" : _) -> do
        SQL.withTransaction conn $ M08_MoveIsExpenseColumn.migrate conn
      [] -> do
        die "Missing argument"
      args -> do
        die $ "Invalid migration number, args:" <> show args
