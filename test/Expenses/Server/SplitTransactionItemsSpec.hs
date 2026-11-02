module Expenses.Server.SplitTransactionItemsSpec where

import Config qualified
import CustomPrelude
import Data.Time (fromGregorian)
import Database qualified as Db
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time qualified as Time
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Server.Routes.GetTransactions (NewShortTransactionItem (..))
import Expenses.Server.Routes.SplitTransactionItems (splitTransactionItemsHandler)
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..))
import Servant (ServerError)
import Test.Syd (Spec, describe, expectationFailure, it, shouldBe)
import Types

spec :: Spec
spec = describe "splitTransactionItemsHandler" do
  it "copies the suggestion to every new item" do
    items <- splitAndGetItems (Just "JOYIN Kit de Slime para Festas\n64GB MP3 Player para Crianças Bluetooth 5.3")
    map (.detailsSuggestion) items
      `shouldBe` [ Just "JOYIN Kit de Slime para Festas\n64GB MP3 Player para Crianças Bluetooth 5.3"
                 , Just "JOYIN Kit de Slime para Festas\n64GB MP3 Player para Crianças Bluetooth 5.3"
                 ]

  it "does not add a suggestion when the transaction has none" do
    items <- splitAndGetItems Nothing
    map (.detailsSuggestion) items `shouldBe` [Nothing, Nothing]

{- | Inserts a transaction with the given suggestion, splits it in 2 items,
and returns the items stored in the db afterwards.
-}
splitAndGetItems :: Maybe Text -> IO [TransactionItemRecord]
splitAndGetItems detailsSuggestion = do
  env <- Util.mkTestEnv
  conn <- Util.mkInMemoryDbConn

  let
    row =
      Db.TransactionJoinedRow
        { transactionId = "tx1"
        , account = "bank"
        , date = fromGregorian 2026 10 30
        , desc = "COMPRA AMAZON EU SARL LUXEMBOURG"
        , totalAmountCents = BECents (-59_98)
        , isExpense = True
        , itemIndex = 0
        , itemAmountCents = BECents (-59_98)
        , tag = Nothing
        , details = ""
        , detailsSuggestion
        }
    splitItems =
      [ NewShortTransactionItem{itemAmountCents = FECents 19_99, details = "", tag = "toys", isExpense = True}
      , NewShortTransactionItem{itemAmountCents = FECents 39_99, details = "", tag = "electronics", isExpense = True}
      ]

  result <-
    do
      SQL.useConnection \c -> Db.insertTransactionJoinedRow c row
      void $ splitTransactionItemsHandler Config.cronUser "tx1" splitItems
      SQL.useConnection \c -> Db.getTransactionById c "tx1"
      & runErrorNoCallStack @ServerError
      & SQL.runSQLiteSync conn
      & Time.runTime
      & EventLog.runEventLog
      & runReader env
      & runConcurrent
      & runLog "test" mempty LogAttention
      & runEff

  case result of
    Right (Just txRecord) -> pure txRecord.items
    Right Nothing -> expectationFailure "Transaction not found after split" >> pure []
    Left err -> expectationFailure (show err) >> pure []
