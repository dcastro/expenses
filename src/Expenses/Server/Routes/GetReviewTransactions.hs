module Expenses.Server.Routes.GetReviewTransactions where

import CustomPrelude
import Data.Time (addGregorianMonthsClip, utctDay)
import Database qualified as Db
import Effectful
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Server.Routes.GetTransactions (TransactionItem, convertRowToItem)

-- | How far back the "Review" page goes.
reviewMonths :: Integer
reviewMonths = 6

{- | Returns the transaction items from the last `reviewMonths` months that the user still has to review:
items without a tag, or with a pending suggestion.
Includes both expenses and non-expenses.
-}
getReviewTransactionsHandler ::
  (Time :> es, SQLite :> es) =>
  Eff es [TransactionItem]
getReviewTransactionsHandler = do
  today <- utctDay <$> Time.currentTime
  let startDate = addGregorianMonthsClip (negate reviewMonths) today
  useConnection \conn ->
    Db.getReviewTransactions conn startDate
      <&> fmap (\row -> convertRowToItem row)
