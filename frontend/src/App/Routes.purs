module App.Routes where

import Routing.Duplex

import Core.APITypes (TagName(..), TagParams(..))
import Core.APITypes as API
import Core.Sorting (SortedOrder(..), SortingColumn(..))
import Core.Sorting as Sorting
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Prelude (class Eq, class Show, ($), (<<<), (<>), (==), (>>>))
import Record as Record
import Routing.Duplex.Generic (sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser as Parser
import Type.Proxy (Proxy(..))

{-
 Resources:
   * https://discourse.purescript.org/t/introducing-halogen-router-one-stop-shop-for-managing-routing-in-halogen-app/2922
   * https://github.com/katsujukou/purescript-halogen-router
   * https://github.com/katsujukou/purescript-halogen-router/tree/main/example
-}
data AppRoute
  = SingleMonth ModalFlag
  | MonthRange ModalFlag
  | Search SearchAppRoute

derive instance Eq AppRoute
derive instance Generic AppRoute _

instance Show AppRoute where
  show = genericShow

type SearchRoute =
  { allFields :: String
  , date :: String
  , account :: Maybe String
  , desc :: String
  , amount :: String
  , tag :: Maybe TagParams
  , notes :: String
  , isExpense :: Maybe Boolean
  , column :: SortingColumn
  , order :: SortedOrder
  }

-- Docs: https://pursuit.purescript.org/packages/purescript-routing-duplex/
routeCodec :: RouteDuplex' AppRoute
routeCodec = root $ sum
  { "SingleMonth": "single-month" / modalParams
  , "MonthRange": "month-range" / modalParams
  , "Search": "search" / searchParamsCodec
  }
  where
  modalParams :: RouteDuplex' ModalFlag
  modalParams =
    params
      { newTx: flag }

defaultModalFlag :: ModalFlag
defaultModalFlag = { newTx: false }

defaultSingleMonthRoute :: AppRoute
defaultSingleMonthRoute = SingleMonth defaultModalFlag

defaultMonthRangeRoute :: AppRoute
defaultMonthRangeRoute = MonthRange defaultModalFlag

mkSearchAppRoute :: SearchRoute -> Boolean -> AppRoute
mkSearchAppRoute searchRoute isOpen =
  Search { searchRoute, newTx: isOpen }

isSingleMonth :: AppRoute -> Boolean
isSingleMonth = case _ of
  SingleMonth _ -> true
  _ -> false

isMonthRange :: AppRoute -> Boolean
isMonthRange = case _ of
  MonthRange _ -> true
  _ -> false

isSearchRoute :: AppRoute -> Boolean
isSearchRoute = case _ of
  Search _ -> true
  _ -> false

isModalOpen :: AppRoute -> Boolean
isModalOpen = case _ of
  SingleMonth flag -> flag.newTx
  MonthRange flag -> flag.newTx
  Search record -> record.newTx

setModalOpen :: Boolean -> AppRoute -> AppRoute
setModalOpen isOpen = case _ of
  SingleMonth flag -> SingleMonth (flag { newTx = isOpen })
  MonthRange flag -> MonthRange (flag { newTx = isOpen })
  Search record -> Search (record { newTx = isOpen })

getSearchRoute :: AppRoute -> Maybe SearchRoute
getSearchRoute = case _ of
  Search record -> Just record.searchRoute
  _ -> Nothing

type ModalFlag =
  { newTx :: Boolean
  }

type SearchAppRoute =
  { searchRoute :: SearchRoute
  , newTx :: Boolean
  }

-- The search route is first deserialized to this record, and then converted to `SearchAppRoute`
type SearchCodecRecord =
  { allFields :: String
  , date :: String
  , account :: Maybe String
  , desc :: String
  , amount :: String
  , tag :: Maybe TagParams
  , notes :: String
  , isExpense :: Maybe Boolean
  , column :: SortingColumn
  , order :: SortedOrder
  , newTx :: Boolean
  }

searchParamsCodec :: RouteDuplex' SearchAppRoute
searchParamsCodec =
  dimap toCodec fromCodec
    $ params
        { allFields: nonemptystring
        , date: nonemptystring
        , account: string >>> optional
        , desc: nonemptystring
        , amount: nonemptystring
        , tag: tagParams >>> optional
        , notes: nonemptystring
        , isExpense: boolean >>> optional
        , column: sortingColumn
        , order: sortedOrder
        , newTx: flag
        }
  where
  toCodec :: SearchAppRoute -> SearchCodecRecord
  toCodec { searchRoute, newTx } =
    Record.merge searchRoute { newTx }

  fromCodec :: SearchCodecRecord -> SearchAppRoute
  fromCodec codecRecord =
    let
      searchRoute :: SearchRoute
      searchRoute = Record.delete (Proxy :: Proxy "newTx") codecRecord
    in
      { searchRoute
      , newTx: codecRecord.newTx
      }

tagParams :: RouteDuplex' String -> RouteDuplex' TagParams
tagParams = as toStr fromStr
  where
  toStr :: TagParams -> String
  toStr = case _ of
    NoTag -> "notag"
    SomeTag tag -> API.getTagName tag

  fromStr :: String -> Either String TagParams
  fromStr = case _ of
    "notag" -> Right NoTag
    str -> Right $ SomeTag (TagName str)

nonemptystring :: RouteDuplex' String -> RouteDuplex' String
nonemptystring =
  omitting ""

sortingColumn :: RouteDuplex' String -> RouteDuplex' SortingColumn
sortingColumn = omitting (toStr Sorting.defaultSorted.column) >>> as toStr fromStr
  where
  toStr :: SortingColumn -> String
  toStr = case _ of
    Date -> "date"
    Amount -> "amount"
    Tag -> "tag"

  fromStr :: String -> Either String SortingColumn
  fromStr = case _ of
    "date" -> Right Date
    "amount" -> Right Amount
    "tag" -> Right Tag
    other -> Left $ "Invalid `SortingColumn`: '" <> other <> "'"

sortedOrder :: RouteDuplex' String -> RouteDuplex' SortedOrder
sortedOrder = omitting (toStr Sorting.defaultSorted.order) >>> as toStr fromStr
  where
  toStr :: SortedOrder -> String
  toStr = case _ of
    Asc -> "asc"
    Desc -> "desc"

  fromStr :: String -> Either String SortedOrder
  fromStr = case _ of
    "asc" -> Right Asc
    "desc" -> Right Desc
    other -> Left $ "Invalid `SortedOrder`: '" <> other <> "'"

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- When parsing: If the query string does not contain this parameter, default it to `omitVal`.
-- When encoding: If the value is `omitVal`, then don't include this parameter in the query string.
omitting :: String -> RouteDuplex' String -> RouteDuplex' String
omitting omitVal =
  string >>> optional >>> omitting'
  where
  omitting' :: RouteDuplex' (Maybe String) -> RouteDuplex' String
  omitting' = as' printErr encode decode
    where
    printErr :: Maybe String -> String
    printErr = fromMaybe "<missing>"

    encode :: String -> Maybe String
    encode str =
      if str == omitVal then Nothing
      else Just str

    decode :: Maybe String -> Either String String
    decode mb = Right $ fromMaybe omitVal mb

-- Similar to `Duplex.as`, but allows the input to be *any* type, and not just `String`.
-- (In the use case above, the input is `Maybe String`)
--
-- The user must provide an additional `printErr` function to convert
-- the input type `s` to `String` for error reporting.
as' :: forall s a b. (s -> String) -> (a -> s) -> (s -> Either String b) -> RouteDuplex s s -> RouteDuplex a b
as' printErr f g (RouteDuplex enc dec) = RouteDuplex (enc <<< f) (Parser.as printErr g dec)
