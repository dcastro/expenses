-- | Lists the transactions from the last 6 months that the user still has to review:
-- | transactions without a tag, or with a pending suggestion.
module App.Review where

import Prelude

import App.TransactionsTable as TransactionsTable
import Core.API as API
import Core.APITypes (TagName, TransactionItem)
import Data.Array as Arr
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import HtmlUtils (classes')
import Type.Proxy (Proxy(..))

type Slot id = forall query. H.Slot query Void id

type Slots =
  ( transactionsTable :: TransactionsTable.Slot Unit
  )

_transactionsTable = Proxy :: Proxy "transactionsTable"

type Input =
  { isAdmin :: Boolean
  , allTags :: Array TagName
  }

type Output = Void

type State =
  { isAdmin :: Boolean
  , allTags :: Array TagName
  -- | `Nothing` until the transactions are loaded.
  , transactions :: Maybe (Array TransactionItem)
  }

data Action
  = Initialize
  | HandleTransactionsUpdated TransactionsTable.Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \{ isAdmin, allTags } ->
        { isAdmin
        , allTags
        , transactions: Nothing
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.section []
    [ HH.section [ classes' "section" ]
        [ HH.h4 [ classes' "title is-4 has-text-centered" ]
            [ HH.text "Review" ]
        , HH.p [ classes' "subtitle is-6 has-text-centered has-text-grey" ]
            [ HH.text "Transactions from the last 6 months without a tag, or with a suggested description." ]
        , case state.transactions of
            Nothing ->
              HH.p [ classes' "has-text-grey has-text-centered" ] [ HH.text "Loading..." ]
            Just txs ->
              HH.p [ classes' "has-text-centered" ]
                [ HH.text $ show (Arr.length txs) <> " to review" ]
        ]
    , HH.section [ classes' "section is-fullheight" ]
        [ HH.slot
            _transactionsTable
            unit
            TransactionsTable.component
            { transactions: fromMaybe [] state.transactions
            , isAdmin: state.isAdmin
            , allTags: state.allTags
            }
            HandleTransactionsUpdated
        ]
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize ->
    refresh

  -- Reload the list, so that the transactions that the user has just reviewed disappear.
  HandleTransactionsUpdated TransactionsTable.TransactionsUpdated ->
    refresh

refresh :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
refresh = do
  transactions <- H.liftAff API.getReviewTransactions
  H.modify_ _ { transactions = Just transactions }
