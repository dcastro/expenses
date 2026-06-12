{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.Ntfy (
  Ntfy (..),
  runNtfy,
  clearNotifications,
  sendNotification,
  Notification (..),
  NtfyMessage (..),
  NtfyAction (..),
) where

import Control.Concurrent.MVar qualified as IOMVar
import CustomPrelude
import Data.Aeson (FromJSON, Options (..), ToJSON, Value, camelTo2, defaultOptions)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import Expenses.Server.Env (Env (..))
import GHC.IO (unsafePerformIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Servant.API hiding ((:>))
import Servant.API qualified as Servant
import Servant.Client

----------------------------------------------------------------------------
-- Servant interface
-- See: https://docs.ntfy.sh/publish/
----------------------------------------------------------------------------

{- | The id of a notification.
We can use it to clear or update it later.

https://docs.ntfy.sh/publish/#updating-notifications
-}
newtype SequenceId = SequenceId Text
  deriving newtype (Eq, Show, ToJSON, FromJSON, ToHttpApiData)

data NtfyAction = NtfyAction
  { action :: Text
  , label :: Text
  , clear :: Bool
  , url :: Text
  }
  deriving stock (Eq, Show)

data NtfyMessage = NtfyMessage
  { topic :: Text
  , title :: Text
  , message :: Text
  , priority :: Int
  , tags :: [Text]
  , click :: Text
  , actions :: [NtfyAction]
  }
  deriving stock (Eq, Show)

data NtfyMessageResponse = NtfyMessageResponse
  { id :: SequenceId
  }
  deriving stock (Eq, Show)

$( mconcat
     [ deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NtfyAction
     , deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NtfyMessage
     , deriveFromJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NtfyMessageResponse
     ]
 )

type Routes :: forall k. k -> Type
data Routes routes = Routes
  { publish ::
      routes
        :- ReqBody '[JSON] NtfyMessage
          Servant.:> Post '[JSON] NtfyMessageResponse
  , clear ::
      routes
        :- Capture "topic" Text
          Servant.:> Capture "sequenceId" SequenceId
          Servant.:> "clear"
          Servant.:> Put '[JSON] Value
  }
  deriving stock (Generic)

type API = NamedRoutes Routes

api :: Proxy API
api = Proxy

routes :: Routes (AsClientT ClientM)
routes = client api

publishNotification :: NtfyMessage -> ClientM NtfyMessageResponse
publishNotification msg =
  routes // (.publish) /: msg

clearNotificationsClient :: Text -> SequenceId -> ClientM Value
clearNotificationsClient topic sequenceId =
  routes
    // (.clear)
    /: topic
    /: sequenceId

runNtfyClient :: Manager -> ClientM a -> IO a
runNtfyClient manager act = do
  let env = mkClientEnv manager (BaseUrl Https "ntfy.sh" 443 "")
  runClientM act env >>= \case
    Right a -> pure a
    Left err -> throwM err

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

data Notification = Notification
  { title :: Text
  , message :: Text
  , clickUrl :: Text
  }
  deriving stock (Eq, Show)

data Ntfy :: Effect where
  ClearNotifications :: Ntfy m ()
  SendNotification :: Notification -> Ntfy m ()

makeEffect ''Ntfy

-- An MVar to keep track of the "sequence ID" of the last notification we sent.
{-# NOINLINE lastSequenceIdVar #-}
lastSequenceIdVar :: MVar (Maybe SequenceId)
lastSequenceIdVar =
  -- See: https://wiki.haskell.org/index.php?oldid=64612
  unsafePerformIO $ IOMVar.newMVar Nothing

runNtfy :: (Reader Env :> es, IOE :> es, Concurrent :> es) => Eff (Ntfy : es) a -> Eff es a
runNtfy action = do
  manager <- liftIO $ Client.newManager TLS.tlsManagerSettings
  env <- ask @Env

  action & interpret \_ -> \case
    ClearNotifications -> do
      -- If we've previously sent a notification, clear it. If not, do nothing.
      MVar.modifyMVar_ lastSequenceIdVar \case
        Nothing -> do
          pure Nothing
        Just seqId -> do
          liftIO $ runNtfyClient manager do
            clearNotificationsClient env.ntfyTopic seqId
          pure Nothing
    SendNotification notification -> do
      rsp <- liftIO $ runNtfyClient manager do
        publishNotification $ mkNtfyMessage env.ntfyTopic notification
      -- Save the Sequence ID of this notification, so that we can clear it later if needed.
      void $ MVar.swapMVar lastSequenceIdVar (Just rsp.id)

mkNtfyMessage :: Text -> Notification -> NtfyMessage
mkNtfyMessage topic notification =
  NtfyMessage
    { topic
    , title = notification.title
    , message = notification.message
    , priority = 5
    , -- https://docs.ntfy.sh/emojis/
      tags = ["warning", "bangbang"]
    , click = notification.clickUrl
    , actions =
        [ NtfyAction
            { action = "view"
            , label = "View Budget"
            , clear = False
            , url = notification.clickUrl
            }
        ]
    }
