module Expenses.Server.Routes.GetAvailableDateRange where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Time.Calendar.Month (Month)
import Database qualified as Db
import Expenses.Server.AppM (AppM, useConnection)
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
getAvailableDateRangeHandler :: AppM DateRange
getAvailableDateRangeHandler = do
  maybeRange <-
    useConnection \conn ->
      liftIO $ Db.getTransactionsMonthRange conn
  case maybeRange of
    Just (minMonth, maxMonth) -> pure $ DateRange{minMonth, maxMonth}
    Nothing -> throwJsonError err404 ("No transactions available" :: Text)
