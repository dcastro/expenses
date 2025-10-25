module Expenses.Server.Routes.Search where

import Control.Lens hiding (Contains)
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.List (partition)
import Data.Text qualified as T
import Database (AmountParams (..), Contains (..), DoesNotContain (..), IsGTE (..), IsLT (..), SearchParams (..), StringParams (..), TagParams)
import Database qualified as Db
import Expenses.NonEmptyText qualified as NET
import Expenses.Server.AppM (AppM, useConnection)
import Expenses.Server.Routes.GetTransactions qualified as GetTransactions

data RawSearchParams = RawSearchParams
  { allFields :: Text
  , date :: Text
  , account :: Maybe Text
  , desc :: Text
  , amount :: Text
  , tag :: Maybe TagParams
  , notes :: Text
  , isExpense :: Maybe Bool
  }

$( mconcat
     [ deriveFromJSON defaultOptions ''RawSearchParams
     ]
 )

searchHandler :: RawSearchParams -> AppM (Vector GetTransactions.TransactionItem)
searchHandler params = do
  let parsedParams = parseSearchParams params

  if parsedParams == emptyParams
    then pure mempty
    else do
      useConnection \conn -> do
        rows <- Db.search conn parsedParams
        pure $ rows <&> \row -> GetTransactions.convertRowToItem row

parseSearchParams :: RawSearchParams -> SearchParams
parseSearchParams sp = do
  let (allFields, transactionId) = parseStringParamsOrId sp.allFields
  SearchParams
    { allFields
    , transactionId
    , date = parseContains sp.date
    , account = sp.account
    , desc = parseStringParams sp.desc
    , amount = parseAmountParams sp.amount
    , tag = sp.tag
    , notes = parseStringParams sp.notes
    , isExpense = sp.isExpense
    }

-- >>> parseAmountParams "  >   100.3  "
-- Just (AmountIsGTE 100.3)
--
-- >>> parseAmountParams ">100"
-- Just (AmountIsGTE 100.0)
--
-- >>> parseAmountParams "<50"
-- Just (AmountIsLT 50.0)
--
-- >>> parseAmountParams "50"
-- Just (AmountContains "50")
parseAmountParams :: Text -> Maybe AmountParams
parseAmountParams str = do
  let (prefix, suffix) = T.splitAt 1 $ T.strip str
  case prefix of
    ">" -> do
      number <- readMaybe suffix
      Just $ AmountIsGTE $ IsGTE number
    "<" -> do
      number <- readMaybe suffix
      Just $ AmountIsLT $ IsLT number
    _ -> parseContains str <&> AmountContains

parseContains :: Text -> Maybe Contains
parseContains str = NET.fromText str <&> Contains

-- >>> parseStringParams " aa   bb -cc dd -ee "
-- StringParams {contains = ["aa","bb","dd"], doesNotContain = ["cc","ee"]}
--
-- >>> parseStringParams ""
-- StringParams {contains = [], doesNotContain = []}
parseStringParams :: Text -> StringParams
parseStringParams input =
  let (doesNotContain, contains) = partition (T.isPrefixOf "-") (words input)
      stripMinus w = T.drop 1 w
   in StringParams
        { contains =
            contains
              & mapMaybe NET.fromText
              <&> Contains
        , doesNotContain =
            doesNotContain
              <&> stripMinus
              & mapMaybe NET.fromText
              <&> DoesNotContain
        }

-- >>> parseStringParamsOrId " aa   bb -cc dd -ee "
-- (StringParams {contains = ["aa","bb","dd"], doesNotContain = ["cc","ee"]},Nothing)
--
-- >>> parseStringParamsOrId " aa   bb -cc dd -ee id:\"abc\"  "
-- (StringParams {contains = ["aa","bb","dd"], doesNotContain = ["cc","ee"]},Just "\"abc\"")
--
-- >>> parseStringParamsOrId " id:\"abc\"  "
-- (StringParams {contains = [], doesNotContain = []},Just "\"abc\"")
--
-- >>> parseStringParamsOrId ""
-- (StringParams {contains = [], doesNotContain = []},Nothing)
parseStringParamsOrId :: Text -> (StringParams, Maybe Text)
parseStringParamsOrId input =
  let
    (txId, (doesNotContain, contains)) =
      input
        & words
        & partition (T.isPrefixOf "id:")
        & second (partition (T.isPrefixOf "-"))
    stripMinus w = T.drop 1 w
    stripId w = T.drop 3 w
   in
    ( StringParams
        { contains =
            contains
              & mapMaybe NET.fromText
              <&> Contains
        , doesNotContain =
            doesNotContain
              <&> stripMinus
              & mapMaybe NET.fromText
              <&> DoesNotContain
        }
    , txId & safeHead <&> stripId
    )

emptyParams :: SearchParams
emptyParams =
  SearchParams
    { allFields = StringParams [] []
    , transactionId = Nothing
    , date = Nothing
    , account = Nothing
    , desc = StringParams [] []
    , amount = Nothing
    , tag = Nothing
    , notes = StringParams [] []
    , isExpense = Nothing
    }
