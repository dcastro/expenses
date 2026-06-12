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

import CustomPrelude
import Data.Aeson (Options (..), Value, camelTo2, defaultOptions)
import Data.Aeson.TH (deriveToJSON)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH (makeEffect)
import Expenses.Server.Env (Env (..))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
-- Hide servant's `:>`, which clashes with effectful's `:>`.
import Servant.API hiding ((:>))
import Servant.API qualified as Servant
import Servant.Client

----------------------------------------------------------------------------
-- Servant interface
-- See: https://docs.ntfy.sh/publish/
----------------------------------------------------------------------------

data NtfyAction = NtfyAction
  { action :: Text
  , label :: Text
  , clear :: Bool
  , url :: Text
  }
  deriving stock (Eq, Show)

data NtfyMessage = NtfyMessage
  { sequenceId :: Text
  , topic :: Text
  , title :: Text
  , message :: Text
  , priority :: Int
  , tags :: [Text]
  , click :: Text
  , actions :: [NtfyAction]
  }
  deriving stock (Eq, Show)

$( mconcat
     [ deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NtfyAction
     , deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NtfyMessage
     ]
 )

type Routes :: forall k. k -> Type
data Routes routes = Routes
  { publish ::
      routes
        :- ReqBody '[JSON] NtfyMessage
          Servant.:> Post '[JSON] Value
  , clear ::
      routes
        :- Capture "topic" Text
          Servant.:> Capture "sequenceId" Text
          Servant.:> "clear"
          Servant.:> Put '[JSON] Value
  }
  deriving stock (Generic)

type API = NamedRoutes Routes

api :: Proxy API
api = Proxy

routes :: Routes (AsClientT ClientM)
routes = client api

publishNotification :: NtfyMessage -> ClientM Value
publishNotification msg =
  routes // (.publish) /: msg

clearNotificationsClient :: Text -> Text -> ClientM Value
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

-- | Notifications published with the same sequence ID replace one another,
-- and can all be cleared at once.
ntfySequenceId :: Text
ntfySequenceId = "xOxg5AKKyjK3"

runNtfy :: (Reader Env :> es, IOE :> es) => Eff (Ntfy : es) a -> Eff es a
runNtfy action = do
  manager <- liftIO $ Client.newManager TLS.tlsManagerSettings
  env <- ask @Env

  action & interpret \_ -> \case
    ClearNotifications ->
      void $ liftIO $ runNtfyClient manager do
        clearNotificationsClient env.ntfyTopic ntfySequenceId
    SendNotification notification ->
      void $ liftIO $ runNtfyClient manager do
        publishNotification (mkNtfyMessage env.ntfyTopic notification)

mkNtfyMessage :: Text -> Notification -> NtfyMessage
mkNtfyMessage topic notification =
  NtfyMessage
    { sequenceId = ntfySequenceId
    , topic
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
