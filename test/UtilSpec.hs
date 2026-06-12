module UtilSpec where

import Config (AppConfig (..), BudgetConfig (..), PushNotificationsConfig (..))
import CustomPrelude
import Expenses.Server.CronJobs.Sync qualified as CronJob
import Expenses.Test.Util ()
import Test.Tasty.HUnit
import Universum.Unsafe qualified as Unsafe
import Util

unit_eurosToCents :: IO ()
unit_eurosToCents = do
  Util.eurosToCents "10" @?= 1000
  Util.eurosToCents "10.00" @?= 1000
  Util.eurosToCents "10.5" @?= 1050
  Util.eurosToCents "10.55" @?= 1055
  Util.eurosToCents "0.99" @?= 99
  Util.eurosToCents "0" @?= 0
  Util.eurosToCents "0.0" @?= 0
  Util.eurosToCents "123.4" @?= 12340
  Util.eurosToCents "123.45" @?= 12345
  Util.eurosToCents "  7.01 " @?= 701
  Util.eurosToCents "7" @?= 700
  Util.eurosToCents "7.1" @?= 710
  Util.eurosToCents "7.123" @?= 712

  -- With a `-`
  Util.eurosToCents "-10" @?= -1000
  Util.eurosToCents "-10.00" @?= -1000
  Util.eurosToCents "-10.5" @?= -1050
  Util.eurosToCents "-10.55" @?= -1055
  Util.eurosToCents "-0.99" @?= -99
  Util.eurosToCents "-0" @?= 0
  Util.eurosToCents "-0.0" @?= 0
  Util.eurosToCents "-123.4" @?= -12340
  Util.eurosToCents "-123.45" @?= -12345
  Util.eurosToCents "  -7.01 " @?= -701
  Util.eurosToCents "-7" @?= -700
  Util.eurosToCents "-7.1" @?= -710
  Util.eurosToCents "-7.123" @?= -712

  -- With a `+`
  Util.eurosToCents "+10" @?= 1000
  Util.eurosToCents "+10.00" @?= 1000
  Util.eurosToCents "+10.5" @?= 1050
  Util.eurosToCents "+10.55" @?= 1055
  Util.eurosToCents "+0.99" @?= 99
  Util.eurosToCents "+0" @?= 0
  Util.eurosToCents "+0.0" @?= 0
  Util.eurosToCents "+123.4" @?= 12340
  Util.eurosToCents "+123.45" @?= 12345
  Util.eurosToCents "  +7.01 " @?= 701
  Util.eurosToCents "+7" @?= 700
  Util.eurosToCents "+7.1" @?= 710
  Util.eurosToCents "+7.123" @?= 712

  -- Google Sheets adds a thousands separator
  Util.eurosToCents "1,283.33" @?= 128333
  Util.eurosToCents "+1,283.33" @?= 128333
  Util.eurosToCents "-1,283.33" @?= -128333

unit_getIsExpense :: IO ()
unit_getIsExpense = do
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
                      { cronSchedule = "30 9 */5 * *"
                      , openUrl = ""
                      }
                }
          }
  let txIdNormal = "20220121233851916940"
  let txIdTemporary = "12345678901234"
  let tagTaxIsExpense = "groceries"
  let !txTagNotExpense = Unsafe.head config.notExpenses

  -- Normal txId, normal tag
  CronJob.getIsExpense config (Just txIdNormal) (Just tagTaxIsExpense) @?= True
  -- txDesc contains notExpenses pattern
  CronJob.getIsExpense config (Just txIdNormal) (Just txTagNotExpense) @?= False
  -- Temporary txId
  CronJob.getIsExpense config (Just txIdTemporary) (Just tagTaxIsExpense) @?= False
