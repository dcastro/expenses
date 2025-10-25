module Expenses.Server.EventLog where

import CustomPrelude
import Data.Aeson (Encoding)
import Data.Aeson.Encoding qualified as J
import Data.ByteString.Lazy qualified as BSL
import Data.Monoid.Linear qualified as Linear
import Data.Time (Day, UTCTime (..))
import Expenses.Linear (LinearToJSON, linearCoerce, (.=))
import Expenses.Linear qualified as Linear
import Expenses.Server.AppM (Env (..))
import Types (Admin, BECents (..), TagName (..))

data Action = Action
  { username :: Admin
  , ts :: UTCTime
  , transactionId :: Text
  , transactionDesc :: Text
  , itemIndex :: Int
  , actionType :: ActionType
  }

data ActionType
  = UpdateTag UpdateTag
  | UpdateIsExpense UpdateIsExpense
  | UpdateDetails UpdateDetails
  | NewTx NewTx
  | AddItem AddedItem
  | RemoveItem RemovedItem

data UpdateTag = MkUpdateTag
  { old :: Maybe TagName
  , new :: TagName
  }

data UpdateIsExpense = MkUpdateIsExpense
  { old :: Bool
  , new :: Bool
  }

data UpdateDetails = MkUpdateDetails
  { old :: Text
  , new :: Text
  }

data NewTx = MkNewTx
  { account :: Text
  , date :: Day
  , totalAmountCents :: BECents
  , isExpense :: Bool
  , tag :: Maybe TagName
  , details :: Text
  }

data AddedItem = MkAddedItem
  { itemAmountCents :: BECents
  , tag :: TagName
  , details :: Text
  , isExpense :: Bool
  }

data RemovedItem = MkRemovedItem
  { itemAmountCents :: BECents
  , tag :: Maybe TagName
  , details :: Text
  , isExpense :: Bool
  }

encoding :: Action %1 -> Encoding
encoding
  ( Action
      { username
      , ts
      , transactionId
      , transactionDesc
      , itemIndex
      , actionType
      }
    ) = do
    case actionType of
      UpdateTag (MkUpdateTag{old, new}) ->
        encUpdate "UpdateTag" username ts transactionId transactionDesc itemIndex old new
      UpdateIsExpense (MkUpdateIsExpense{old, new}) ->
        encUpdate "UpdateIsExpense" username ts transactionId transactionDesc itemIndex old new
      UpdateDetails (MkUpdateDetails{old, new}) ->
        encUpdate "UpdateDetails" username ts transactionId transactionDesc itemIndex old new
      NewTx (MkNewTx{account, date, totalAmountCents, isExpense, tag, details}) -> do
        Linear.pairs
          ( Linear.mconcat
              [ "ts" .= ts
              , "user" .= username
              , "type" .= ("NewTx" :: Text)
              , "desc" .= transactionDesc
              , "tx" .= transactionId
              , "idx" .= itemIndex
              , "account" .= account
              , "date" .= date
              , "totalAmountCents" .= linearCoerce @BECents @Int totalAmountCents
              , "isExpense" .= isExpense
              , "tag" .= tag
              , "details" .= details
              ]
          )
      AddItem (MkAddedItem{itemAmountCents, tag, details, isExpense}) ->
        encItem "AddItem" username ts transactionId transactionDesc itemIndex itemAmountCents (Just tag) details isExpense
      RemoveItem (MkRemovedItem{itemAmountCents, tag, details, isExpense}) ->
        encItem "RemoveItem" username ts transactionId transactionDesc itemIndex itemAmountCents tag details isExpense

encUpdate ::
  forall oldT newT.
  (LinearToJSON oldT, LinearToJSON newT) =>
  Text %1 ->
  Admin %1 ->
  UTCTime %1 ->
  Text %1 ->
  Text %1 ->
  Int %1 ->
  oldT %1 ->
  newT %1 ->
  Encoding
encUpdate
  actionTag
  username
  ts
  transactionId
  transactionDesc
  itemIndex
  oldVal
  newVal =
    Linear.pairs
      ( Linear.mconcat
          [ "ts" .= ts
          , "user" .= username
          , "type" .= actionTag
          , "desc" .= transactionDesc
          , "old" .= oldVal
          , "new" .= newVal
          , "tx" .= transactionId
          , "idx" .= itemIndex
          ]
      )

encItem ::
  Text %1 ->
  Admin %1 ->
  UTCTime %1 ->
  Text %1 ->
  Text %1 ->
  Int %1 ->
  BECents %1 ->
  Maybe TagName %1 ->
  Text %1 ->
  Bool %1 ->
  Encoding
encItem
  actionTag
  username
  ts
  transactionId
  transactionDesc
  itemIndex
  (BECents amount)
  tag
  details
  isExpense =
    Linear.pairs
      ( Linear.mconcat
          [ "ts" .= ts
          , "user" .= username
          , "type" .= actionTag
          , "desc" .= transactionDesc
          , "tx" .= transactionId
          , "idx" .= itemIndex
          , "itemAmountCents" .= amount
          , "tag" .= tag
          , "details" .= details
          , "isExpense" .= isExpense
          ]
      )

append :: (MonadIO m, MonadReader Env m) => Action -> m ()
append action = do
  env <- ask
  let bs = J.encodingToLazyByteString $ encoding action
  liftIO $ BSL.appendFile env.eventLogPath (bs <> "\n")
