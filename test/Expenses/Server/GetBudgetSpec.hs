module Expenses.Server.GetBudgetSpec where

import Config
import Control.Lens
import CustomPrelude
import Data.Map.Strict qualified as Map
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Vector qualified as V
import Database qualified as Db
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Log (runLog)
import Effectful.Reader.Static (runReader)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time qualified as Time
import Expenses.Server.Env
import Expenses.Server.Routes.GetBudget
import Expenses.Server.Routes.GetTransactions (TransactionItem (..))
import Expenses.Server.Utils (MapAsList (..))
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..), mkBulkLogger)
import Test.Hspec (Spec, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Types

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
    }

spec_mkBudgetTagGroupStats :: Spec
spec_mkBudgetTagGroupStats = it "groups transactions by tag group" do
  let
    groups =
      [ Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = BECents -650_00}
      , Config.BudgetTagGroup{name = "Transport", tags = ["fuel", "parking"], limitCents = BECents -100_00}
      , Config.BudgetTagGroup{name = "Other", tags = ["takeaway"], limitCents = BECents -50_00}
      ]
    txs =
      V.fromList
        [ mkItem "tx" (fromGregorian 2026 6 1) (Just "groceries") 50_00
        , mkItem "tx" (fromGregorian 2026 6 1) (Just "groceries") 1_00
        , mkItem "tx" (fromGregorian 2026 6 1) (Just "fuel") 20_00
        , mkItem "tx" (fromGregorian 2026 6 1) (Just "parking") 2_00
        , mkItem "tx" (fromGregorian 2026 6 1) (Just "takeaway") 5_00
        , mkItem "tx" (fromGregorian 2026 6 1) (Just "random") 10_00
        , mkItem "tx" (fromGregorian 2026 6 1) (Just "random") 3_00
        , mkItem "tx" (fromGregorian 2026 6 1) Nothing 15_00
        , mkItem "tx" (fromGregorian 2026 6 1) Nothing 4_00
        ]
    expected =
      Map.fromList
        [ ("Groceries", BudgetTagGroupStats{spentToDateCents = 51_00, limitCents = 650_00, tags = [Just "groceries"]})
        , ("Transport", BudgetTagGroupStats{spentToDateCents = 22_00, limitCents = 100_00, tags = [Just "fuel", Just "parking"]})
        , ("Other", BudgetTagGroupStats{spentToDateCents = 37_00, limitCents = 50_00, tags = [Just "takeaway", Just "random", Nothing]})
        ]
  mkBudgetTagGroupStats groups txs `shouldBe` expected

  -- The sum of the groups should equal the total spending
  let totalGroupSpending = expected <&> (.spentToDateCents) & sum
  let totalSpending = txs <&> (.itemAmountCents) & sum
  totalGroupSpending `shouldBe` totalSpending

spec_getBudgetHandler :: Spec
spec_getBudgetHandler = it "returns correct budget info for the current month" do
  env <-
    Util.mkTestEnv <&> \env ->
      env
        & config . budget
          .~ BudgetConfig
            { tagGroups =
                [ Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = BECents -650_00}
                , Config.BudgetTagGroup{name = "Go out", tags = ["go out"], limitCents = BECents -100_00}
                , Config.BudgetTagGroup{name = "Other", tags = ["casa", "eletronica", "jogos"], limitCents = BECents -100_00}
                ]
            , includeAllTxsFromAccounts = mempty
            }
  conn <- Util.mkInMemoryDbConn

  let testRows =
        [ mkRow "tx1" (fromGregorian 2026 6 5) (Just "groceries") 50_00
        , mkRow "tx2" (fromGregorian 2026 6 10) (Just "go out") 20_00
        , mkRow "tx3" (fromGregorian 2026 6 3) (Just "jogos") 30_00
        , -- tx4 is in May → excluded by the "06-2026" date filter
          mkRow "tx4" (fromGregorian 2026 5 15) (Just "groceries") 10_00
        ]

  nullLogger <- mkBulkLogger "null" (\_ -> pure ()) (pure ())

  forM_ testRows \row ->
    SQL.useConnection (\c -> Db.insertTransactionJoinedRow c row)
      & SQL.runSQLiteSync conn
      & runConcurrent
      & runEff

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
        [ mkItem "tx1" (fromGregorian 2026 6 5) (Just "groceries") 50_00
        , mkItem "tx2" (fromGregorian 2026 6 10) (Just "go out") 20_00
        , mkItem "tx3" (fromGregorian 2026 6 3) (Just "jogos") 30_00
        ]

  let expectedTagGroupStats =
        MapAsList $
          Map.fromList
            [ ("Groceries", BudgetTagGroupStats{spentToDateCents = 50_00, limitCents = 650_00, tags = [Just "groceries"]})
            , ("Go out", BudgetTagGroupStats{spentToDateCents = 20_00, limitCents = 100_00, tags = [Just "go out"]})
            ,
              ( "Other"
              , BudgetTagGroupStats
                  { spentToDateCents = 30_00
                  , limitCents = 100_00
                  , tags = [Just "casa", Just "eletronica", Just "jogos"]
                  }
              )
            ]

  sortTxs resp.transactions `shouldBe` sortBy (comparing (.transactionId)) expectedTransactions
  resp.monthlyLimitCents `shouldBe` 850_00
  resp.expectedSpendingToDateCents `shouldBe` 425_00
  resp.actualSpendingToDateCents `shouldBe` 100_00
  resp.overUnderCents `shouldBe` (-325_00)
  resp.tagGroupStats `shouldBe` expectedTagGroupStats
