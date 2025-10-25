module Expenses.Server.Routes.ModifyTransaction where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Time.Clock (getCurrentTime)
import Database qualified as Db
import Expenses.Server.AppM (AppM, Env, useConnection)
import Expenses.Server.EventLog
import Expenses.Server.EventLog qualified as EventLog
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Expenses.Server.Utils (throwJsonError)
import Servant (err500)
import Types (Admin (..), TagName)

data ModifyTransaction = ModifyTransaction
  { transactionId :: Text
  , itemIndex :: Int
  , actionType :: ModifyTransactionType
  }

data ModifyTransactionType
  = ModifyTag ModifyTag
  | ModifyIsExpense ModifyIsExpense
  | ModifyDetails ModifyDetails

data ModifyTag = MkModifyTag
  { tag :: TagName
  }

data ModifyIsExpense = MkModifyIsExpense
  { isExpense :: Bool
  }

data ModifyDetails = MkModifyDetails
  { details :: Text
  }

$( mconcat
     [ deriveFromJSON defaultOptions ''ModifyTransaction
     , deriveFromJSON defaultOptions ''ModifyTransactionType
     , deriveFromJSON defaultOptions ''ModifyTag
     , deriveFromJSON defaultOptions ''ModifyIsExpense
     , deriveFromJSON defaultOptions ''ModifyDetails
     ]
 )

-- | Modifies a transaction / transaction item and returns the updated transaction items.
modifyTransactionHandler :: Admin -> ModifyTransaction -> AppM GetTransactions.TransactionItem
modifyTransactionHandler admin ModifyTransaction{transactionId = txId, itemIndex, actionType} = do
  now <- liftIO getCurrentTime

  let appendAction :: forall m. (MonadIO m, MonadReader Env m) => Text -> ActionType -> m ()
      appendAction txDesc actionType =
        EventLog.append
          Action
            { username = admin
            , ts = now
            , transactionId = txId
            , transactionDesc = txDesc
            , itemIndex
            , actionType
            }

  useConnection \conn -> do
    desc <- Db.getDescription conn txId

    case actionType of
      ModifyTag (MkModifyTag newTag) -> do
        oldTag <- Db.getTag conn txId itemIndex
        when (Just newTag /= oldTag) do
          Db.updateTag conn txId itemIndex newTag
          appendAction desc $ UpdateTag MkUpdateTag{old = oldTag, new = newTag}
      ModifyIsExpense (MkModifyIsExpense newIsExpense) -> do
        oldIsExpense <- Db.getIsExpense conn txId itemIndex
        when (oldIsExpense /= newIsExpense) do
          Db.updateIsExpense conn txId itemIndex newIsExpense
          appendAction desc $ UpdateIsExpense MkUpdateIsExpense{old = oldIsExpense, new = newIsExpense}
      ModifyDetails (MkModifyDetails newDetails) -> do
        oldDetails <- Db.getDetails conn txId itemIndex
        when (oldDetails /= newDetails) do
          Db.updateDetails conn txId itemIndex newDetails
          appendAction desc $ UpdateDetails MkUpdateDetails{old = oldDetails, new = newDetails}

    rowMb <- Db.getTransactionItemById conn txId itemIndex
    case rowMb of
      Nothing -> throwJsonError err500 [i|Failed to find tx item after modifying it: #{txId} index #{itemIndex}|]
      Just row -> pure $ GetTransactions.convertRowToItem row
