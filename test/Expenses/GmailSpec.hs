{- | NOTE: the Gmail API fixtures in `test/fixtures/gmail/` are hand-written,
following the documented format of `users.messages.get` (`format=full`) and `users.messages.list`.
-}
module Expenses.GmailSpec where

import CustomPrelude
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Log (runLog)
import Expenses.Effects.Gmail (AccessToken (..), Gmail (..), GmailSettings (..), RefreshToken (..), authCodeForm, refreshTokenForm)
import Expenses.Email (Email (..))
import Expenses.Gmail
import Gmail (GmailMessage (..), MessageList (..), MessagePart (..), MessageRef (..))
import Log (LogLevel (..), LogMessage (..), mkLogger, waitForLogger)
import Test.Syd (Spec, describe, it, shouldBe)
import Web.FormUrlEncoded (Form, lookupUnique)

spec :: Spec
spec = do
  describe "toEmail" do
    it "uses the text/plain part, and ignores the HTML part and the attachment" do
      email <- toEmail "someone@gmail.com" <$> fixture "multipart"
      email
        `shouldBe` Email
          { account = "someone@gmail.com"
          , messageId = "18f1a2b3c4d5e6f7"
          , from = "Amazon.es <confirmar-envio@amazon.es>"
          , subject = "Chega amanhã: \"JOYIN Kit de Slime para Festas...\" e mais 1 artigo"
          , date = "Tue, 28 Oct 2026 18:12:04 +0000"
          , body =
              T.intercalate
                "\n"
                [ "Chega amanhã"
                , "Diogo - Rio Tinto"
                , "N.º de pedido 404-4893280-7691528"
                , ""
                , "Acompanhar a encomenda"
                , ""
                , "JOYIN Kit de Slime para Festas, 36 Pacotes de Slime de Galáxia para Lembrancinhas de Festa - elástico, não pegajoso, Livre de Sujeira, Alívio do Estresse, e Seguro para Meninas e Meninos"
                , ""
                , "Vendido por Joyin Direct"
                , ""
                , "Quantidade: 1"
                , ""
                , "1999€"
                ]
          }

    it "converts the text/html part when there is no text/plain part, and finds lowercase headers" do
      email <- toEmail "someone@gmail.com" <$> fixture "html-only"
      (email.subject, email.date) `shouldBe` ("A sua encomenda Amazon.es", "Wed, 29 Oct 2026 09:40:51 +0000")
      email.body
        `shouldBe` T.intercalate
          "\n"
          [ "Pré-visualização"
          , ""
          , "Obrigado pela sua encomenda, Diogo!"
          , ""
          , "Cabo USB-C para USB-C 2m, Carregamento Rápido 100W PD & Nylon"
          , "Quantidade: 1"
          , ""
          , "12,49 €"
          , ""
          , "Total da encomenda: 12,49 €"
          ]

    it "decodes the charset from the Content-Type header" do
      email <- toEmail "someone@gmail.com" <$> fixture "latin1"
      email.body `shouldBe` "Calças de ganga Slim Fit\nPreço: 29,90 EUR"

    it "cuts long bodies" do
      email <- toEmail "someone@gmail.com" <$> fixture "long"
      T.length email.body `shouldBe` maxBodyChars

  describe "MessageList" do
    it "decodes a page" do
      page <- fixture @MessageList "list-page"
      page `shouldBe` MessageList{messages = Just [MessageRef "m1", MessageRef "m2"], nextPageToken = Just "page-2"}

    it "decodes an empty result, which has no `messages`" do
      page <- fixture @MessageList "list-empty"
      page `shouldBe` MessageList{messages = Nothing, nextPageToken = Nothing}

  describe "fetchEmails" do
    it "follows the pages" do
      (emails, calls, logs) <- runFetch 10
      map (.messageId) emails `shouldBe` ["m1", "m2", "m3"]
      calls `shouldBe` ["token", "list Nothing", "list Just \"page-2\"", "get m1", "get m2", "get m3"]
      logs `shouldBe` []

    it "stops at the maximum, and logs a warning" do
      (emails, calls, logs) <- runFetch 2
      map (.messageId) emails `shouldBe` ["m1", "m2"]
      calls `shouldBe` ["token", "list Nothing", "get m1", "get m2"]
      logs `shouldBe` ["[Gmail] More than 2 e-mails in someone@gmail.com match 'newer_than:14d'. Only the newest 2 are used."]

  describe "token forms" do
    let settings = GmailSettings{clientId = "client-id", clientSecret = "client-secret"}
    it "exchanges an authorization code" do
      let form = authCodeForm settings "https://example.com/gmail/callback" "the-code"
      map (field form) ["grant_type", "code", "redirect_uri", "client_id", "client_secret"]
        `shouldBe` map Right ["authorization_code", "the-code", "https://example.com/gmail/callback", "client-id", "client-secret"]
    it "refreshes an access token" do
      let form = refreshTokenForm settings (RefreshToken "the-refresh-token")
      map (field form) ["grant_type", "refresh_token", "client_id", "client_secret"]
        `shouldBe` map Right ["refresh_token", "the-refresh-token", "client-id", "client-secret"]

  describe "RefreshToken" do
    it "does not show the token" do
      show @Text (RefreshToken "the-refresh-token") `shouldBe` "RefreshToken <hidden>"
 where
  field :: Form -> Text -> Either Text Text
  field form name = lookupUnique name form

fixture :: forall a. (FromJSON a) => FilePath -> IO a
fixture name =
  eitherDecodeFileStrict @a ("test/fixtures/gmail/" <> name <> ".json")
    >>= either (\err -> error [i|Failed to decode fixture #{name}: #{err}|]) pure

{- | Runs `fetchEmails` with a fake `Gmail`. There are 2 pages: [m1, m2] and [m3].
Returns the e-mails, the calls to `Gmail`, and the log messages.
-}
runFetch :: Int -> IO ([Email], [Text], [Text])
runFetch maxMessages = do
  calls <- newIORef []
  logs <- newIORef []
  logger <- mkLogger "test" \msg -> modifyIORef' logs (<> [msg.lmMessage])
  let record call = liftIO $ modifyIORef' calls (<> [call])
  emails <-
    fetchEmails "someone@gmail.com" (RefreshToken "refresh") "newer_than:14d" maxMessages
      & interpret
        ( \_ -> \case
            GetAccessToken (RefreshToken "refresh") -> record "token" $> AccessToken "access"
            GetAccessToken _ -> error "Unexpected refresh token"
            ListMessages _ _ pageToken -> do
              record [i|list #{show @Text pageToken}|]
              pure $ case pageToken of
                Nothing -> MessageList{messages = Just [MessageRef "m1", MessageRef "m2"], nextPageToken = Just "page-2"}
                Just _ -> MessageList{messages = Just [MessageRef "m3"], nextPageToken = Nothing}
            GetMessage _ messageId -> do
              record [i|get #{messageId}|]
              pure $ GmailMessage{id = messageId, payload = MessagePart "text/plain" "" [] (Just "SGk") []}
            _ -> error "Unexpected call"
        )
      & runLog "test" logger LogInfo
      & runEff
  -- The logger writes the messages in another thread.
  waitForLogger logger
  (emails,,) <$> readIORef calls <*> readIORef logs
