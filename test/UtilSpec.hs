module UtilSpec where

import Config (AppConfig (..), BudgetConfig (..), PushNotificationsConfig (..))
import CustomPrelude
import Expenses.Server.CronJobs.Sync qualified as CronJob
import Expenses.Test.Util ()
import Test.Syd (Spec, it, shouldBe)
import Universum.Unsafe qualified as Unsafe
import Util

spec :: Spec
spec = do
  it "eurosToCents" eurosToCentsTest
  it "getIsExpense" getIsExpenseTest

eurosToCentsTest :: IO ()
eurosToCentsTest = do
  Util.eurosToCents "10" `shouldBe` 1000
  Util.eurosToCents "10.00" `shouldBe` 1000
  Util.eurosToCents "10.5" `shouldBe` 1050
  Util.eurosToCents "10.55" `shouldBe` 1055
  Util.eurosToCents "0.99" `shouldBe` 99
  Util.eurosToCents "0" `shouldBe` 0
  Util.eurosToCents "0.0" `shouldBe` 0
  Util.eurosToCents "123.4" `shouldBe` 12340
  Util.eurosToCents "123.45" `shouldBe` 12345
  Util.eurosToCents "  7.01 " `shouldBe` 701
  Util.eurosToCents "7" `shouldBe` 700
  Util.eurosToCents "7.1" `shouldBe` 710
  Util.eurosToCents "7.123" `shouldBe` 712

  -- With a `-`
  Util.eurosToCents "-10" `shouldBe` -1000
  Util.eurosToCents "-10.00" `shouldBe` -1000
  Util.eurosToCents "-10.5" `shouldBe` -1050
  Util.eurosToCents "-10.55" `shouldBe` -1055
  Util.eurosToCents "-0.99" `shouldBe` -99
  Util.eurosToCents "-0" `shouldBe` 0
  Util.eurosToCents "-0.0" `shouldBe` 0
  Util.eurosToCents "-123.4" `shouldBe` -12340
  Util.eurosToCents "-123.45" `shouldBe` -12345
  Util.eurosToCents "  -7.01 " `shouldBe` -701
  Util.eurosToCents "-7" `shouldBe` -700
  Util.eurosToCents "-7.1" `shouldBe` -710
  Util.eurosToCents "-7.123" `shouldBe` -712

  -- With a `+`
  Util.eurosToCents "+10" `shouldBe` 1000
  Util.eurosToCents "+10.00" `shouldBe` 1000
  Util.eurosToCents "+10.5" `shouldBe` 1050
  Util.eurosToCents "+10.55" `shouldBe` 1055
  Util.eurosToCents "+0.99" `shouldBe` 99
  Util.eurosToCents "+0" `shouldBe` 0
  Util.eurosToCents "+0.0" `shouldBe` 0
  Util.eurosToCents "+123.4" `shouldBe` 12340
  Util.eurosToCents "+123.45" `shouldBe` 12345
  Util.eurosToCents "  +7.01 " `shouldBe` 701
  Util.eurosToCents "+7" `shouldBe` 700
  Util.eurosToCents "+7.1" `shouldBe` 710
  Util.eurosToCents "+7.123" `shouldBe` 712

  -- Google Sheets adds a thousands separator
  Util.eurosToCents "1,283.33" `shouldBe` 128333
  Util.eurosToCents "+1,283.33" `shouldBe` 128333
  Util.eurosToCents "-1,283.33" `shouldBe` -128333

getIsExpenseTest :: IO ()
getIsExpenseTest = do
  let config =
        AppConfig
          { institutions = []
          , admins = []
          , allTagGroups = mempty
          , ungroupedTags = mempty
          , cronSchedule = ""
          , tagPatterns = mempty
          , notExpenses = ["transfers"]
          , budget =
              BudgetConfig
                { tagGroups = []
                , includeAllTxsFromAccounts = mempty
                , pushNotifications =
                    PushNotificationsConfig
                      { cronSchedule = ""
                      , openUrl = ""
                      }
                }
          }
  let txIdNormal = "20220121233851916940"
  let txIdTemporary = "12345678901234"
  let tagTaxIsExpense = "groceries"
  let !txTagNotExpense = Unsafe.head config.notExpenses

  -- Normal txId, normal tag
  CronJob.getIsExpense config (Just txIdNormal) (Just tagTaxIsExpense) `shouldBe` True
  -- txDesc contains notExpenses pattern
  CronJob.getIsExpense config (Just txIdNormal) (Just txTagNotExpense) `shouldBe` False
  -- Temporary txId
  CronJob.getIsExpense config (Just txIdTemporary) (Just tagTaxIsExpense) `shouldBe` False
