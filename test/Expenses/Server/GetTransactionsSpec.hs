module Expenses.Server.GetTransactionsSpec where

import CustomPrelude
import Data.Aeson.Encode.Pretty qualified as J
import Data.Map.Strict qualified as Map
import Data.Time (MonthOfYear, fromGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)
import Database qualified as Db
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite.Simple qualified as SQL
import Expenses.Server.Routes.GetTransactions
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Utils (MapAsList (..))
import Expenses.Test.Util ()
import Expenses.Test.Util qualified as Util
import Test.Syd (Spec, goldenLazyByteStringFile, it, shouldBe)
import Types

spec :: Spec
spec = do
  specMkGroupStats
  specGetTransactionsHandler

specMkGroupStats :: Spec
specMkGroupStats = it "calculates group and tag stats" do
  config <- Util.mkTestConfig
  let
    mkTransactionRow :: MonthOfYear -> Maybe TagName -> BECents -> Db.TransactionJoinedRow
    mkTransactionRow monthOfYear tag amt =
      Db.TransactionJoinedRow
        { transactionId = "tid"
        , account = "acc"
        , date = fromGregorian 2024 monthOfYear 1
        , desc = "desc"
        , totalAmountCents = 1_000_000
        , isExpense = True
        , itemAmountCents = amt
        , tag = tag
        , details = "details"
        , itemIndex = 0
        }

    expected =
      [ TagGroupStats
          { name = "Groceries"
          , groupTotalAmountCents = 6000
          , groupPercentage = 60
          , tags =
              [ TagStats "groceries" 6000 100 60 $
                  MapAsList $
                    Map.fromList
                      [ (YearMonth 2024 1, 6000)
                      ]
              ]
          }
      , TagGroupStats
          { name = "Electronics"
          , groupTotalAmountCents = 3000
          , groupPercentage = 30
          , tags =
              [ TagStats "games" 2400 80 24 $
                  MapAsList $
                    Map.fromList
                      [ (YearMonth 2024 1, 2000)
                      , (YearMonth 2024 2, 400)
                      ]
              , TagStats "electronics" 600 20 6 $
                  MapAsList $
                    Map.fromList
                      [ (YearMonth 2024 1, 600)
                      ]
              ]
          }
      , TagGroupStats
          { name = "Other"
          , groupTotalAmountCents = 1000
          , groupPercentage = 10
          , tags =
              [ TagStats "bbb" 750 75 7 $
                  MapAsList $
                    Map.fromList
                      [ (YearMonth 2024 1, 750)
                      ]
              , TagStats "aaa" 250 25 2 $
                  MapAsList $
                    Map.fromList
                      [ (YearMonth 2024 1, 250)
                      ]
              ]
          }
      ]
    rows =
      [ mkTransactionRow 1 (Just "groceries") -6000
      , mkTransactionRow 1 (Just "electronics") -600
      , mkTransactionRow 1 (Just "games") -2000
      , mkTransactionRow 2 (Just "games") -300
      , mkTransactionRow 2 (Just "games") -100
      , mkTransactionRow 1 (Just "aaa") -250
      , mkTransactionRow 1 (Just "bbb") -750
      , mkTransactionRow 1 Nothing -900
      ]

  let items = rows <&> \row -> GetTransactions.convertRowToItem row
  let itemsWithTags =
        items & mapMaybe \tx -> do
          tag <- tx.tag
          Just (tx, tag)
  mkGroupStats config itemsWithTags `shouldBe` expected

specGetTransactionsHandler :: Spec
specGetTransactionsHandler =
  it "mkGroupStats golden test" $
    goldenLazyByteStringFile "test/golden/getTransactionsHandler.json" do
      env <- Util.mkTestEnv
      conn <- Util.mkTestDbConn
      resp <-
        getTransactionsHandler (YearMonth 2025 08) (YearMonth 2025 09)
          & SQL.runSQLiteSync conn
          & runReader env
          & runConcurrent
          & runEff
      pure $ J.encodePretty resp
