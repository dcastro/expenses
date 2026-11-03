{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Reads the user's e-mails with the Gmail API. See `docs/plans/email-description-suggestions.md`.
module Expenses.Effects.Gmail (
  Gmail (..),
  exchangeAuthCode,
  getAccessToken,
  getProfileEmail,
  listMessages,
  getMessage,
  GmailSettings (..),
  RefreshToken (..),
  AccessToken (..),
  GmailError (..),
  runGmail,
  authCodeForm,
  refreshTokenForm,
) where

import CustomPrelude
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import GHC.Show (Show (showsPrec), showString)
import Gmail qualified as G
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Network.HTTP.Types (statusCode)
import Servant.Auth.Client qualified as SA
import Servant.Client (ClientError (..), ClientM, ResponseF (..), baseUrlHost)
import Servant.Client.Core (RequestF (..))
import Web.FormUrlEncoded (Form, toForm)

-- | A long-lived token that gives read-only access to one mailbox. A secret.
newtype RefreshToken = RefreshToken Text
  deriving stock (Eq)

instance Show RefreshToken where
  showsPrec _ _ = showString "RefreshToken <hidden>"

-- | A short-lived token for the Gmail API. A secret.
newtype AccessToken = AccessToken Text
  deriving stock (Eq)

instance Show AccessToken where
  showsPrec _ _ = showString "AccessToken <hidden>"

data Gmail :: Effect where
  -- | Exchanges the code from Google's OAuth redirect for a refresh token.
  ExchangeAuthCode ::
    -- | The redirect URI used in the authorization request.
    Text ->
    -- | The code.
    Text ->
    Gmail m RefreshToken
  GetAccessToken :: RefreshToken -> Gmail m AccessToken
  -- | The address of the mailbox that the token gives access to.
  GetProfileEmail :: AccessToken -> Gmail m Text
  ListMessages ::
    AccessToken ->
    -- | A Gmail search query, e.g. "newer_than:14d".
    Text ->
    -- | The page token from the previous page.
    Maybe Text ->
    Gmail m G.MessageList
  GetMessage :: AccessToken -> Text -> Gmail m G.GmailMessage

makeEffect ''Gmail

data GmailSettings = GmailSettings
  { clientId :: Text
  , clientSecret :: Text
  }

newtype GmailError = GmailError Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The number of messages per page of `users.messages.list`.
pageSize :: Int
pageSize = 100

runGmail :: forall es a. (IOE :> es) => GmailSettings -> Eff (Gmail : es) a -> Eff es a
runGmail settings action = do
  manager <- liftIO $ Client.newManager TLS.tlsManagerSettings
  let
    oauth :: ClientM b -> Eff es b
    oauth = liftIO . (G.runOAuth manager >=> rethrow)
    gmail :: ClientM b -> Eff es b
    gmail = liftIO . (G.runGmailClient manager >=> rethrow)

  action & interpret \_ -> \case
    ExchangeAuthCode redirectUri code -> do
      rsp <- oauth $ G.postToken $ authCodeForm settings redirectUri code
      case rsp.refreshToken of
        Just refreshToken -> pure $ RefreshToken refreshToken
        Nothing ->
          throwM
            $ GmailError
              "Google did not return a refresh token. The authorization request must have `access_type=offline` and `prompt=consent`."
    GetAccessToken refreshToken -> do
      rsp <- oauth $ G.postToken $ refreshTokenForm settings refreshToken
      pure $ AccessToken rsp.accessToken
    GetProfileEmail token ->
      gmail (G.getProfile (bearer token)) <&> (.emailAddress)
    ListMessages token query pageToken ->
      gmail $ G.listMessages (bearer token) query pageSize pageToken
    GetMessage token messageId ->
      gmail $ G.getMessage (bearer token) messageId
 where
  bearer :: AccessToken -> SA.Token
  bearer (AccessToken token) = SA.Token $ encodeUtf8 token

  {- Servant's `FailureResponse` contains the request's headers, i.e. the access token.
  So we don't rethrow it as is: we keep only the status, the host and the response body.
  -}
  rethrow :: Either ClientError b -> IO b
  rethrow = \case
    Right b -> pure b
    Left (FailureResponse req rsp) -> do
      let
        (baseUrl, path) = req.requestPath
        host = baseUrlHost baseUrl
        status = statusCode rsp.responseStatusCode
        body = T.take 2000 $ decodeUtf8 @Text $ BSL.toStrict rsp.responseBody
      throwM $ GmailError [i|Request to #{host} #{path} failed with status #{status}: #{body}|]
    Left (DecodeFailure err _) -> throwM $ GmailError [i|Failed to decode Google's response: #{err}|]
    Left (ConnectionError err) -> throwM $ GmailError [i|Failed to connect to Google: #{displayException err}|]
    Left err -> throwM $ GmailError [i|Request to Google failed: #{T.take 2000 (show err)}|]

authCodeForm :: GmailSettings -> Text -> Text -> Form
authCodeForm settings redirectUri code =
  toForm @[(Text, Text)]
    [ ("grant_type", "authorization_code")
    , ("code", code)
    , ("redirect_uri", redirectUri)
    , ("client_id", settings.clientId)
    , ("client_secret", settings.clientSecret)
    ]

refreshTokenForm :: GmailSettings -> RefreshToken -> Form
refreshTokenForm settings (RefreshToken refreshToken) =
  toForm @[(Text, Text)]
    [ ("grant_type", "refresh_token")
    , ("refresh_token", refreshToken)
    , ("client_id", settings.clientId)
    , ("client_secret", settings.clientSecret)
    ]
