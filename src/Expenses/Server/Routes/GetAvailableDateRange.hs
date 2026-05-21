module Expenses.Server.Routes.GetAvailableDateRange where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Time.Calendar.Month (Month)
import Database qualified as Db
import Effectful
import Expenses.Effects
import Expenses.Server.AppM (useConnection)
import Expenses.Server.Utils (throwJsonError)
import Servant (err404)

data DateRange = DateRange
  { minMonth :: Month
  , maxMonth :: Month
  }

$( mconcat
     [ deriveToJSON defaultOptions ''DateRange
     ]
 )

-- | Returns the date range for which we have data available.
getAvailableDateRangeHandler ::
  (Db :> es, Reader Env :> es, Concurrent :> es, Error ServerError :> es) =>
  Eff es DateRange
getAvailableDateRangeHandler = do
  maybeRange <-
    useConnection \conn ->
      Db.getTransactionsMonthRange conn
  case maybeRange of
    Just (minMonth, maxMonth) -> pure $ DateRange{minMonth, maxMonth}
    Nothing -> throwJsonError err404 ("No transactions available" :: Text)
