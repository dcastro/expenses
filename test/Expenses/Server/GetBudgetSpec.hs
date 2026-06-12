module Expenses.Server.GetBudgetSpec where

import Config
import Control.Lens
import CustomPrelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Utils (MapAsList (..))
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..), mkBulkLogger)
import Test.Hspec (Spec, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Types

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

mkRow :: Text -> Day -> Text -> Maybe TagName -> FECents -> Db.TransactionJoinedRow
mkRow txId date accountName tag amt =
  Db.TransactionJoinedRow
    { transactionId = txId
    , account = accountName
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
      [ Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = 650_00}
      , Config.BudgetTagGroup{name = "Transport", tags = ["fuel", "parking"], limitCents = 100_00}
      , Config.BudgetTagGroup{name = "Other", tags = ["takeaway", "gifts"], limitCents = 50_00}
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
        -- Txs with the tag "groceries" were put in the "Groceries" group.
        [ ("Groceries", BudgetTagGroupStats{spentToDateCents = 51_00, limitCents = 650_00, tags = [Just "groceries"]})
        , -- Txs with the tag "fuel" OR "parking" were put in the "Transport" group.
          ("Transport", BudgetTagGroupStats{spentToDateCents = 22_00, limitCents = 100_00, tags = [Just "fuel", Just "parking"]})
        , -- Txs with the tag "takeaway"  were put in the "Other" group.
          -- There are no txs with the tags "gifts", but that tag is still listed in the group info because it's declared in the config.
          -- The "random" tag is not declared in the config, so those txs were also put in the "Other" group.
          -- The tx with no tag was also put in the "Other" group.
          ("Other", BudgetTagGroupStats{spentToDateCents = 37_00, limitCents = 50_00, tags = [Just "takeaway", Just "gifts", Just "random", Nothing]})
        ]
  mkBudgetTagGroupStats groups txs `shouldBe` expected

  -- The sum of the groups should equal the total spending
  let totalGroupSpending = expected <&> (.spentToDateCents) & sum
  let totalSpending = txs <&> (.itemAmountCents) & sum
  totalGroupSpending `shouldBe` totalSpending

spec_getBudgetHandler :: Spec
spec_getBudgetHandler = it "returns correct budget info for the current month" do
  -- Get the budget info for 2026-06-15
  let
    frozenTime = UTCTime (fromGregorian 2026 6 15) (secondsToDiffTime 0)

  env <-
    Util.mkTestEnv <&> \env ->
      env
        & config . budget
          .~ BudgetConfig
            { tagGroups =
                [ Config.BudgetTagGroup{name = "Groceries", tags = ["groceries"], limitCents = 650_00}
                , Config.BudgetTagGroup{name = "Go out", tags = ["go out"], limitCents = 100_00}
                , Config.BudgetTagGroup{name = "Other", tags = ["home", "electronics"], limitCents = 100_00}
                ]
            , includeAllTxsFromAccounts = Set.fromList ["bank1"]
            , pushNotifications =
                PushNotificationsConfig
                  { cronSchedule = "30 9 */5 * *"
                  , openUrl = "http://expenses.example.com/#/budget"
                  }
            }
  conn <- Util.mkInMemoryDbConn

  let
    -- The endpoint returns txs that match the date filter (06-2026) AND (EITHER the tag filter OR the account name filter).
    --
    -- Tx1 matches the tag filter, the account name filter, and the date filter, so it's included in the results.
    tx1 = mkRow "tx1" (fromGregorian 2026 6 5) "bank1" (Just "groceries") 50_00
    -- Tx2 matches the date and tag filter (but not the account name filter), so it's included.
    tx2 = mkRow "tx2" (fromGregorian 2026 6 10) "some-other-bank" (Just "go out") 20_00
    -- Tx3 matches the date and account name filter (but not the tag filter), so it's included.
    tx3 = mkRow "tx3" (fromGregorian 2026 6 3) "bank1" (Just "some-other-tag") 30_00
    -- tx4 is in May, so it's excluded by the date filter
    tx4 = mkRow "tx4" (fromGregorian 2026 5 15) "bank1" (Just "groceries") 10_00
    -- tx5 matches the date filter but not the tag or account filters, so it's excluded.
    tx5 = mkRow "tx5" (fromGregorian 2026 6 3) "some-other-bank" (Just "some-other-tag") 30_00
    testRows = [tx1, tx2, tx3, tx4, tx5]
    expectedTxs = [tx1, tx2, tx3] <&> \tx -> GetTransactions.convertRowToItem tx

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
                  , tags = [Just "home", Just "electronics", Just "some-other-tag"]
                  }
              )
            ]

  let sortTxs = sortBy (comparing (.transactionId))
  sortTxs (V.toList resp.transactions) `shouldBe` sortTxs expectedTxs
  resp.monthlyLimitCents `shouldBe` 850_00
  -- we're halfway through the month, so we expect to have spent half of the monthly limit by now
  resp.expectedSpendingToDateCents `shouldBe` 425_00
  resp.actualSpendingToDateCents `shouldBe` 100_00
  resp.overUnderCents `shouldBe` -325_00
  resp.tagGroupStats `shouldBe` expectedTagGroupStats
