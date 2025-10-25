module Expenses.Server.Routes.InsertNew where

import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Time (Day, UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.UUID qualified as Uuid
import Data.UUID.V4 qualified as Uuid
import Database qualified as Db
import Expenses.Linear qualified as Linear
import Expenses.Server.AppM (AppM, useConnection)
import Expenses.Server.EventLog qualified as EventLog
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions
import Types (Admin (..), FECents, TagName, toBE)

data NewTransactionItem = NewTransactionItem
  { account :: Text
  , date :: Day
  , desc :: Text
  , totalAmountCents :: FECents
  , isExpense :: Bool
  , tag :: Maybe TagName
  , details :: Text
  }
  deriving stock (Show, Eq)

$( mconcat
     [ deriveFromJSON defaultOptions ''NewTransactionItem
     ]
 )

insertTransactionHandler :: Admin -> NewTransactionItem -> AppM GetTransactions.TransactionItem
insertTransactionHandler admin newTxItem = do
  now <- liftIO getCurrentTime
  transactionId <- liftIO mkNewId
  let
    transactionItem = mkTransactionItem transactionId newTxItem
    row = GetTransactions.convertItemToRow transactionItem

  useConnection \conn -> do
    Db.insertTransactionJoinedRow conn row

  EventLog.append $ mkEventLogAction admin now transactionId newTxItem

  pure transactionItem
 where
  mkNewId :: IO Text
  mkNewId = do
    uuid <- Uuid.nextRandom
    pure $ "EXP-" <> Uuid.toText uuid

mkTransactionItem :: Text -> NewTransactionItem %1 -> GetTransactions.TransactionItem
mkTransactionItem transactionId NewTransactionItem{account, date, desc, totalAmountCents, isExpense, tag, details} =
  Linear.liftMove totalAmountCents \totalAmountCents ->
    GetTransactions.TransactionItem
      { transactionId = transactionId
      , account = account
      , date = date
      , desc = desc
      , totalAmountCents = totalAmountCents
      , isExpense = isExpense
      , tag = tag
      , details = details
      , -- defaults
        itemIndex = 0
      , itemAmountCents = totalAmountCents
      }

mkEventLogAction :: Admin %1 -> UTCTime %1 -> Text -> NewTransactionItem %1 -> EventLog.Action
mkEventLogAction
  admin
  now
  transactionId
  NewTransactionItem
    { account
    , date
    , desc
    , totalAmountCents
    , isExpense
    , tag
    , details
    } =
    EventLog.Action
      { username = admin
      , ts = now
      , transactionId = transactionId
      , transactionDesc = desc
      , itemIndex = 0
      , actionType =
          EventLog.NewTx
            EventLog.MkNewTx
              { account = account
              , date = date
              , totalAmountCents = toBE totalAmountCents
              , isExpense = isExpense
              , tag = tag
              , details = details
              }
      }
