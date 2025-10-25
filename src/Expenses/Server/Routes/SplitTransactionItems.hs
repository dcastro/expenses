module Expenses.Server.Routes.SplitTransactionItems where

import CustomPrelude
import Data.Time.Clock (getCurrentTime)
import Database qualified as Db
import Expenses.Server.AppM (AppM, useConnection)
import Expenses.Server.EventLog qualified as EventLog
import Expenses.Server.Routes.GetTransactions (NewShortTransactionItem (..))
import Expenses.Server.Utils (throwJsonError)
import Log (logTrace_)
import Servant (NoContent (..))
import Servant.Server (err400, err404)
import Types (Admin (..), TransactionItemRecord (..), TransactionRecord (..), toBE)

splitTransactionItemsHandler :: Admin -> Text -> [NewShortTransactionItem] -> AppM NoContent
splitTransactionItemsHandler admin transactionId splitItems = do
  -- Load existing transaction items
  originalRecord <-
    useConnection (\conn -> liftIO $ Db.getTransactionById conn transactionId)
      >>= maybe
        (throwJsonError err404 [i|Transaction not found: #{transactionId}|])
        pure

  -- Update record
  let updatedRecord = originalRecord{items = splitItems <&> \i -> mkTransactionItem i}

  -- Sanity check: the sum of all items' amounts should match the total amount
  let
    expectedTotal = originalRecord.totalAmountCents
    actualTotal = updatedRecord.items <&> (.itemAmountCents) & sum
  when (actualTotal /= expectedTotal) do
    throwJsonError err400 [i|Split amounts (#{actualTotal}) do not sum to transaction total (#{expectedTotal})|]

  if originalRecord.items == updatedRecord.items
    then do
      logTrace_ [i|SplitTransactionItems: no changes for #{transactionId}|]
      pure NoContent
    else do
      useConnection \conn -> Db.updateExistingRecord conn updatedRecord
      logTrace_ [i|SplitTransactionItems: updated #{transactionId}|]

      -- Write to the event log
      now <- liftIO getCurrentTime
      for_ (originalRecord.items `zip` [0 ..]) \(item, idx) ->
        EventLog.append
          EventLog.Action
            { username = admin
            , ts = now
            , transactionId = transactionId
            , transactionDesc = updatedRecord.desc
            , itemIndex = idx
            , actionType = mkRemovedActionType item
            }

      for_ (splitItems `zip` [0 ..]) \(item, idx) ->
        EventLog.append
          EventLog.Action
            { username = admin
            , ts = now
            , transactionId = transactionId
            , transactionDesc = updatedRecord.desc
            , itemIndex = idx
            , actionType = mkAddedActionType item
            }
      pure NoContent

mkTransactionItem :: NewShortTransactionItem %1 -> TransactionItemRecord
mkTransactionItem NewShortTransactionItem{itemAmountCents, details, tag, isExpense} =
  TransactionItemRecord
    { itemAmountCents = toBE itemAmountCents
    , tag = Just tag
    , details
    , isExpense
    }

mkAddedActionType :: NewShortTransactionItem %1 -> EventLog.ActionType
mkAddedActionType NewShortTransactionItem{itemAmountCents, details, tag, isExpense} =
  EventLog.AddItem
    EventLog.MkAddedItem
      { itemAmountCents = toBE itemAmountCents
      , tag = tag
      , details
      , isExpense
      }

mkRemovedActionType :: TransactionItemRecord %1 -> EventLog.ActionType
mkRemovedActionType TransactionItemRecord{itemAmountCents, tag, details, isExpense} =
  EventLog.RemoveItem
    EventLog.MkRemovedItem
      { itemAmountCents
      , tag
      , details
      , isExpense
      }
