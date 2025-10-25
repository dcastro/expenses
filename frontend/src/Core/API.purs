module Core.API where

import Core.APITypes
import Prelude

import Affjax (URL, Error, Response)
import Affjax as AJ
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as AJ
import Affjax.Web as AW
import Core.YearMonth (YearMonth)
import Core.YearMonth as YM
import Data.Argonaut as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import HtmlUtils as HtmlUtils
import Partial.Unsafe (unsafeCrashWith)

getHealth :: Aff Unit
getHealth = do
  _ <- get $ HtmlUtils.apiBaseUrl <> "health"
  pure unit

isAdmin :: Aff Boolean
isAdmin = do
  get (HtmlUtils.apiBaseUrl <> "is-admin")
    <#> statusCodeIs 200
    <#> decodeJson

getTransactions :: YearMonth -> YearMonth -> Aff GetTransactions
getTransactions from to = do
  get (HtmlUtils.apiBaseUrl <> "transactions?start=" <> YM.formatYearMonth from <> "&end=" <> YM.formatYearMonth to)
    <#> statusCodeIs 200
    <#> decodeJson

search :: RawSearchParams -> Aff (Array TransactionItem)
search params = do
  let body = RequestBody.json $ J.encodeJson params
  post (Just body) (HtmlUtils.apiBaseUrl <> "search")
    <#> statusCodeIs 200
    <#> decodeJson

allTags :: Aff (Array TagName)
allTags = do
  get (HtmlUtils.apiBaseUrl <> "tags")
    <#> statusCodeIs 200
    <#> decodeJson

allAccounts :: Aff (Array String)
allAccounts = do
  get (HtmlUtils.apiBaseUrl <> "accounts")
    <#> statusCodeIs 200
    <#> decodeJson

getDates :: Aff DateRange
getDates = do
  get (HtmlUtils.apiBaseUrl <> "dates")
    <#> statusCodeIs 200
    <#> decodeJson

createTransaction :: NewTransactionItem -> Aff Unit
createTransaction newTransaction = do
  let body = RequestBody.json $ J.encodeJson newTransaction
  _ <- post (Just body) (HtmlUtils.apiBaseUrl <> "transactions")
    <#> statusCodeIs 200
  pure unit

updateTransaction :: ModifyTransaction -> Aff TransactionItem
updateTransaction body = do
  let reqBody = RequestBody.json $ J.encodeJson body
  put (Just reqBody) (HtmlUtils.apiBaseUrl <> "transactions")
    <#> statusCodeIs 200
    <#> decodeJson

getTransactionItems :: TransactionId -> Aff (Array ShortTransactionItem)
getTransactionItems transactionId = do
  get (HtmlUtils.apiBaseUrl <> "transactions/" <> transactionId <> "/items")
    <#> statusCodeIs 200
    <#> decodeJson

splitTransaction :: TransactionId -> Array NewShortTransactionItem -> Aff Unit
splitTransaction transactionId items = do
  let body = RequestBody.json $ J.encodeJson items
  _ <- post (Just body) (HtmlUtils.apiBaseUrl <> "transactions/" <> transactionId <> "/split")
    <#> statusCodeIs 204
  pure unit

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

get :: URL -> Aff (Either Error (Response J.Json))
get url =
  AW.request AJ.defaultRequest
    { method = Left GET
    , url = url
    , responseFormat = AJ.json
    }

put :: Maybe RequestBody.RequestBody -> URL -> Aff (Either Error (Response J.Json))
put body url =
  AW.request AJ.defaultRequest
    { method = Left PUT
    , url = url
    , responseFormat = AJ.json
    , content = body
    }

post :: Maybe RequestBody.RequestBody -> URL -> Aff (Either Error (Response J.Json))
post body url =
  AW.request AJ.defaultRequest
    { method = Left POST
    , url = url
    , responseFormat = AJ.json
    , content = body
    }

statusCodeIs :: forall a. Int -> Either Error (Response a) -> Response a
statusCodeIs expectedStatusCode result =
  case result of
    Left err -> unsafeCrashWith $ AJ.printError err
    Right response -> do
      if unwrap (response.status) == expectedStatusCode then
        response
      else
        unsafeCrashWith $ "Unexpected status code: " <> show response.status

decodeJson :: forall @a. J.DecodeJson a => Response J.Json -> a
decodeJson response =
  case J.decodeJson response.body of
    Left err -> unsafeCrashWith $ J.printJsonDecodeError err
    Right a -> a
