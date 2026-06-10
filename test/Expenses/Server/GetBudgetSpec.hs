module Expenses.Server.GetBudgetSpec where

import Config qualified
import CustomPrelude
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Vector qualified as V
import Database.SQLite.Simple qualified as SQLiteSimple
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time qualified as Time
import Expenses.Server.Routes.GetBudget
import Expenses.Server.Routes.GetTransactions (TransactionItem (..))
import Expenses.Server.Utils (MapAsList (..))
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..), mkBulkLogger)
import Test.Hspec (Spec, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Types
import Control.Concurrent.MVar qualified as MVar

frozenTime :: UTCTime
frozenTime = UTCTime (fromGregorian 2026 6 15) (secondsToDiffTime 0)

mkItem :: Text -> Day -> Maybe TagName -> FECents -> TransactionItem
mkItem txId date tag amt =
  TransactionItem
    { transactionId = txId
    , itemIndex = 0
    , account = "bank"
    , date
    , desc = "desc"
    , totalAmountCents = amt
    , isExpense = True
    , itemAmountCents = amt
    , tag
    , details = ""
    }

spec_mkBudgetTagGroupStats :: Spec
spec_mkBudgetTagGroupStats = it "groups transactions by budget tag group" do
  let
    groups =
      [ Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = BECents (-65000)}
      , Config.BudgetTagGroup{name = "Transport", tags = ["fuel"], limitCents = BECents (-10000)}
      ]
    txs =
      V.fromList
        [ mkItem "t1" (fromGregorian 2026 6 1) (Just "groceries") (FECents 5000)
        , mkItem "t2" (fromGregorian 2026 6 1) (Just "fuel") (FECents 2000)
        , mkItem "t3" (fromGregorian 2026 6 1) (Just "random") (FECents 1000)
        ]
    expected =
      Map.fromList
        [ ("Groceries", BudgetTagGroupStats{spentToDateCents = FECents 5000, limitCents = FECents 65000, tags = [Just "groceries"]})
        , ("Transport", BudgetTagGroupStats{spentToDateCents = FECents 2000, limitCents = FECents 10000, tags = [Just "fuel"]})
        , ("Other", BudgetTagGroupStats{spentToDateCents = FECents 1000, limitCents = FECents 0, tags = [Just "random"]})
        ]
  mkBudgetTagGroupStats groups txs `shouldBe` expected

spec_getBudgetHandler :: Spec
spec_getBudgetHandler = it "returns correct budget info for the current month" do
  env <- Util.mkTestEnv
  conn <- Util.mkInMemoryDbConn

  -- Seed test data directly into the connection before wrapping in MVar
  MVar.withMVar conn \c -> do
    let insertTx txId date amt =
          SQLiteSimple.execute
            c
            "INSERT INTO transactions (id, account, date, desc, total_amount_cents) VALUES (?,?,?,?,?)"
            (txId :: Text, "bank" :: Text, date :: Text, "desc" :: Text, amt :: Int)
        insertItem txId tag amt =
          SQLiteSimple.execute
            c
            "INSERT INTO transaction_items (transaction_id, item_index, item_amount_cents, tag, details, is_expense) VALUES (?,?,?,?,?,?)"
            (txId :: Text, 0 :: Int, amt :: Int, tag :: Text, "" :: Text, True)

    insertTx "tx1" "2026-06-05" (-5000)
    insertItem "tx1" "groceries" (-5000)
    insertTx "tx2" "2026-06-10" (-2000)
    insertItem "tx2" "go out" (-2000)
    insertTx "tx3" "2026-06-03" (-3000)
    insertItem "tx3" "jogos" (-3000)
    -- tx4 is in May → excluded by the "06-2026" date filter
    insertTx "tx4" "2026-05-15" (-1000)
    insertItem "tx4" "groceries" (-1000)

  nullLogger <- mkBulkLogger "null" (\_ -> pure ()) (pure ())

  resp <-
    getBudgetHandler
      & SQL.runSQLiteSync conn
      & Time.runFrozenTime frozenTime
      & runReader env
      & runConcurrent
      & runLog "test" nullLogger LogAttention
      & runEff

  let sortTxs = sortBy (comparing (.transactionId)) . V.toList

  let expectedTransactions =
        [ mkItem "tx1" (fromGregorian 2026 6 5) (Just "groceries") (FECents 5000)
        , mkItem "tx2" (fromGregorian 2026 6 10) (Just "go out") (FECents 2000)
        , mkItem "tx3" (fromGregorian 2026 6 3) (Just "jogos") (FECents 3000)
        ]

  let expectedTagGroupStats =
        MapAsList $
          Map.fromList
            [ ("Groceries", BudgetTagGroupStats{spentToDateCents = FECents 5000, limitCents = FECents 65000, tags = [Just "groceries"]})
            , ("Go out", BudgetTagGroupStats{spentToDateCents = FECents 2000, limitCents = FECents 10000, tags = [Just "go out"]})
            ,
              ( "Other"
              , BudgetTagGroupStats
                  { spentToDateCents = FECents 3000
                  , limitCents = FECents 10000
                  , tags = [Just "casa", Just "eletronica", Just "jogos"]
                  }
              )
            ]

  sortTxs resp.transactions `shouldBe` sortBy (comparing (.transactionId)) expectedTransactions
  resp.monthlyLimitCents `shouldBe` FECents 85000
  resp.expectedSpendingToDateCents `shouldBe` FECents 42500
  resp.actualSpendingToDateCents `shouldBe` FECents 10000
  resp.overUnderCents `shouldBe` FECents (-32500)
  resp.tagGroupStats `shouldBe` expectedTagGroupStats
