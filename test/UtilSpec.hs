module UtilSpec where

import Config (AppConfig (..))
import CustomPrelude
import Expenses.Server.CronJob qualified as CronJob
import Expenses.Test.Util ()
import Test.Tasty.HUnit
import Types
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
          { accountInfos = []
          , admins = []
          , allTagGroups = mempty
          , cronSchedule = ""
          , categoryPatterns = mempty
          , notExpenses = ["ATM"]
          }
  let accExpense =
        AccountInfo
          { accountId = "exp-acc-id"
          , accountName = "ExpenseAccount"
          , isExpenseAccount = True
          , flipSign = False
          }
  let accNonExpense =
        AccountInfo
          { accountId = "nonexp-acc-id"
          , accountName = "NonExpenseAccount"
          , isExpenseAccount = False
          , flipSign = False
          }
  let txIdNormal = "20220121233851916940"
  let txIdTemporary = "12345678901234"
  let txDescNormal = "Some normal transaction"
  let !txDescNotExpense = "something " <> Unsafe.head config.notExpenses <> " something"

  -- Expense account, normal txId, normal desc
  CronJob.getIsExpense config accExpense txIdNormal txDescNormal @?= True
  -- Non-expense account
  CronJob.getIsExpense config accNonExpense txIdNormal txDescNormal @?= False
  -- Expense account, but txDesc contains notExpenses pattern
  CronJob.getIsExpense config accExpense txIdNormal txDescNotExpense @?= False
  -- Expense account, but temporary txId
  CronJob.getIsExpense config accExpense txIdTemporary txDescNormal @?= False
