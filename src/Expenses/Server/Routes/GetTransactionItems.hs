module Expenses.Server.Routes.GetTransactionItems where

import CustomPrelude hiding (Reader)
import Database qualified as Db
import Effectful
import Expenses.Effects
import Expenses.Server.AppM (useConnection2)
import Expenses.Server.Routes.GetTransactions (ShortTransactionItem (..))
import Expenses.Server.Utils (throwJsonError2)
import Servant (err404)
import Servant qualified as S
import Types (TransactionItemRecord (..), TransactionRecord (..), toFE)

getTransactionItemsHandler ::
  (Reader Env :> es, Concurrent :> es, Db :> es, Error S.ServerError :> es) =>
  Text -> Eff es [ShortTransactionItem]
getTransactionItemsHandler txId = do
  useConnection2 \conn -> do
    txRecord <-
      Db.getTransactionById conn txId
        >>= maybe
          (throwJsonError2 err404 [i|Transaction not found: #{txId}|])
          pure
    pure $ txRecord.items <&> toShortItem
 where
  toShortItem :: TransactionItemRecord -> ShortTransactionItem
  toShortItem item =
    ShortTransactionItem
      { itemAmountCents = toFE item.itemAmountCents
      , tag = item.tag
      , details = item.details
      , isExpense = item.isExpense
      }
