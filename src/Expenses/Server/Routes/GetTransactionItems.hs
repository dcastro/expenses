module Expenses.Server.Routes.GetTransactionItems where

import CustomPrelude
import Database qualified as Db
import Expenses.Server.AppM (AppM, useConnection)
import Expenses.Server.Routes.GetTransactions (ShortTransactionItem (..))
import Expenses.Server.Utils (throwJsonError)
import Servant (err404)
import Types (TransactionItemRecord (..), TransactionRecord (..), toFE)

getTransactionItemsHandler :: Text -> AppM [ShortTransactionItem]
getTransactionItemsHandler txId = do
  useConnection \conn -> do
    txRecord <-
      liftIO (Db.getTransactionById conn txId)
        >>= maybe
          (throwJsonError err404 [i|Transaction not found: #{txId}|])
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
