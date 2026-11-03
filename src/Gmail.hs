{- | A client for Google's OAuth token endpoint, and for the Gmail API.

Docs:
  * https://developers.google.com/identity/protocols/oauth2/web-server
  * https://developers.google.com/workspace/gmail/api/reference/rest/v1/users.messages
-}
module Gmail where

import CustomPrelude
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.TH (deriveFromJSON)
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Auth as S
import Servant.Auth.Client (Token)
import Servant.Client
import Web.FormUrlEncoded (Form)

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | The response of the token endpoint.
data TokenResponse = TokenResponse
  { accessToken :: Text
  , refreshToken :: Maybe Text
  -- ^ Only returned when exchanging an authorization code, with `access_type=offline` and `prompt=consent`.
  }
  deriving stock (Eq, Generic)

newtype Profile = Profile
  { emailAddress :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | One page of `users.messages.list`.
data MessageList = MessageList
  { messages :: Maybe [MessageRef]
  -- ^ Missing when there are no messages.
  , nextPageToken :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype MessageRef = MessageRef
  { id :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | The response of `users.messages.get` with `format=full`. Only the fields we use.
data GmailMessage = GmailMessage
  { id :: Text
  , payload :: MessagePart
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data MessagePart = MessagePart
  { mimeType :: Text
  , filename :: Text
  -- ^ Not empty for attachments.
  , headers :: [MessageHeader]
  , bodyData :: Maybe Text
  -- ^ The part's content, base64url-encoded. Missing for containers (e.g. "multipart/alternative") and attachments.
  , parts :: [MessagePart]
  }
  deriving stock (Eq, Show)

instance FromJSON MessagePart where
  parseJSON = withObject "MessagePart" \o -> do
    mimeType <- o .: "mimeType"
    filename <- o .:? "filename" <&> fromMaybe ""
    headers <- o .:? "headers" <&> fromMaybe []
    bodyData <- o .:? "body" >>= maybe (pure Nothing) (withObject "MessagePartBody" (.:? "data"))
    parts <- o .:? "parts" <&> fromMaybe []
    pure MessagePart{mimeType, filename, headers, bodyData, parts}

data MessageHeader = MessageHeader
  { name :: Text
  , value :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

$(deriveFromJSON (aesonDrop 0 snakeCase) ''TokenResponse)

----------------------------------------------------------------------------
-- OAuth
----------------------------------------------------------------------------

type OAuthRoutes :: forall k. k -> Type
data OAuthRoutes routes = OAuthRoutes
  { token ::
      routes
        :- "token"
          :> ReqBody '[FormUrlEncoded] Form
          :> Post '[JSON] TokenResponse
  }
  deriving stock (Generic)

oauthRoutes :: OAuthRoutes (AsClientT ClientM)
oauthRoutes = client (Proxy @(NamedRoutes OAuthRoutes))

postToken :: Form -> ClientM TokenResponse
postToken form =
  oauthRoutes // (.token) /: form

runOAuth :: Manager -> ClientM a -> IO (Either ClientError a)
runOAuth manager act =
  runClientM act $ mkClientEnv manager (BaseUrl Https "oauth2.googleapis.com" 443 "")

----------------------------------------------------------------------------
-- Gmail
----------------------------------------------------------------------------

type GmailRoutes :: forall k. k -> Type
data GmailRoutes routes = GmailRoutes
  { profile ::
      routes
        :- S.Auth '[JWT] Text
          :> "profile"
          :> Get '[JSON] Profile
  , listMessages ::
      routes
        :- S.Auth '[JWT] Text
          :> "messages"
          :> QueryParam' '[Required, Strict] "q" Text
          :> QueryParam' '[Required, Strict] "maxResults" Int
          :> QueryParam "pageToken" Text
          :> Get '[JSON] MessageList
  , getMessage ::
      routes
        :- S.Auth '[JWT] Text
          :> "messages"
          :> Capture "id" Text
          :> QueryParam' '[Required, Strict] "format" Text
          :> Get '[JSON] GmailMessage
  }
  deriving stock (Generic)

gmailRoutes :: GmailRoutes (AsClientT ClientM)
gmailRoutes = client (Proxy @(NamedRoutes GmailRoutes))

getProfile :: Token -> ClientM Profile
getProfile token =
  gmailRoutes // (.profile) /: token

listMessages :: Token -> Text -> Int -> Maybe Text -> ClientM MessageList
listMessages token query maxResults pageToken =
  gmailRoutes // (.listMessages) /: token /: query /: maxResults /: pageToken

getMessage :: Token -> Text -> ClientM GmailMessage
getMessage token messageId =
  gmailRoutes // (.getMessage) /: token /: messageId /: "full"

runGmailClient :: Manager -> ClientM a -> IO (Either ClientError a)
runGmailClient manager act =
  runClientM act $ mkClientEnv manager (BaseUrl Https "gmail.googleapis.com" 443 "gmail/v1/users/me")
