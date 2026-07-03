module Expenses.Server.BudgetOverrideSpec where

import Config qualified
import CustomPrelude
import Data.Time (Day, fromGregorian)
import Database qualified as Db
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time qualified as Time
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Server.Routes.BudgetOverride
import Expenses.Server.Routes.GetTransactions (TransactionItem (..))
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..))
import Servant (ServerError)
import Test.Syd (Spec, describe, it, shouldBe)
import Types (FECents, TagName, toBE)

mkRow :: Text -> Day -> Maybe TagName -> FECents -> Db.TransactionJoinedRow
mkRow txId date tag amt =
  Db.TransactionJoinedRow
    { transactionId = txId
    , account = "bank"
    , date
    , desc = "desc"
    , totalAmountCents = toBE amt
    , isExpense = True
    , itemIndex = 0
    , itemAmountCents = toBE amt
    , tag
    , details = ""
    , budgetOverride = Nothing
    }

spec :: Spec
spec = describe "budget override handlers" do
  it "sets, then clears, the budget override for a transaction item" do
    env <- Util.mkTestEnv
    conn <- Util.mkInMemoryDbConn
    let admin = Config.cronUser

    let
      -- Runs a handler action against the seeded db, returning the updated item.
      run action =
        action
          & runErrorNoCallStack @ServerError
          & SQL.runSQLiteSync conn
          & Time.runTime
          & EventLog.runEventLog
          & runReader env
          & runConcurrent
          & runLog "test" mempty LogAttention
          & runEff

    SQL.useConnection (\c -> Db.insertTransactionJoinedRow c (mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 50_00))
      & SQL.runSQLiteSync conn
      & runConcurrent
      & runEff

    -- Force-include the item.
    setResult <- run (setBudgetOverrideHandler admin "tx1" 0 (MkSetBudgetOverride True))
    (setResult <&> (.budgetOverride)) `shouldBe` Right (Just True)

    -- Force-exclude the item.
    excludeResult <- run (setBudgetOverrideHandler admin "tx1" 0 (MkSetBudgetOverride False))
    (excludeResult <&> (.budgetOverride)) `shouldBe` Right (Just False)

    -- Clear the override, reverting to the default rules.
    unsetResult <- run (unsetBudgetOverrideHandler admin "tx1" 0)
    (unsetResult <&> (.budgetOverride)) `shouldBe` Right Nothing
