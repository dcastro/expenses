module Expenses.Server.GetTransactionsSpec where

import CustomPrelude
import Data.Aeson.Encode.Pretty qualified as J
import Data.Map.Strict qualified as Map
import Data.Time (MonthOfYear, fromGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)
import Database qualified as Db
import Database.SQLite.Simple qualified as SQL
import Expenses.Server.AppM (Env (..), runLogger)
import Expenses.Server.Routes.GetTransactions
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Utils (MapAsList (..))
import Expenses.Test.Util ()
import GHC.MVar qualified as M
import Servant.Server qualified as Servant
import Test.Hspec (Spec, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Types

spec_mkGroupStats :: Spec
spec_mkGroupStats = it "calculates group and tag stats" do
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
          { name = "Eletronica"
          , groupTotalAmountCents = 3000
          , groupPercentage = 30
          , tags =
              [ TagStats "jogos" 2400 80 24 $
                  MapAsList $
                    Map.fromList
                      [ (YearMonth 2024 1, 2000)
                      , (YearMonth 2024 2, 400)
                      ]
              , TagStats "eletronica" 600 20 6 $
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
      , mkTransactionRow 1 (Just "eletronica") -600
      , mkTransactionRow 1 (Just "jogos") -2000
      , mkTransactionRow 2 (Just "jogos") -300
      , mkTransactionRow 2 (Just "jogos") -100
      , mkTransactionRow 1 (Just "aaa") -250
      , mkTransactionRow 1 (Just "bbb") -750
      , mkTransactionRow 1 Nothing -900
      ]

  let items = rows <&> \row -> GetTransactions.convertRowToItem row
  let itemsWithTags =
        items & mapMaybe \tx -> do
          tag <- tx.tag
          Just (tx, tag)
  mkGroupStats itemsWithTags `shouldBe` expected

test_getTransactionsHandler :: TestTree
test_getTransactionsHandler = do
  goldenVsString "mkGroupStats golden test" "test/golden/getTransactionsHandler.json" do
    let dbPath = "./resources/test-app-dir/expenses.db"
    dbConn <- liftIO $ SQL.open dbPath >>= M.newMVar
    let env =
          Env
            { dbConn
            , eventLogPath = "/dev/null"
            , logsDir = "/dev/null"
            , nordigenSecretId = ""
            , nordigenSecretKey = ""
            }

    resp <-
      Servant.runHandler $
        getTransactionsHandler (YearMonth 2025 08) (YearMonth 2025 09)
          & flip runReaderT env
          & runLogger False mempty
    pure $ J.encodePretty resp
