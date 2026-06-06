module App.Budget where

import Prelude

import App.TransactionsTable as TransactionsTable
import Core.API as API
import Core.APITypes (TagName)
import Core.APITypes as API
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Type.Proxy (Proxy(..))
import Utils as Utils

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
  , budgetInfo :: Maybe API.BudgetInfo
  , loading :: Boolean
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
        , budgetInfo: Nothing
        , loading: false
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
    [ HH.section [ classes' "section is-fullheight" ]
        [ HH.h4 [ classes' "title is-4 has-text-centered" ]
            [ HH.text "Budget" ]
        , if state.loading then
            HH.p [ classes' "has-text-grey" ] [ HH.text "Loading..." ]
          else
            HtmlUtils.displayWhenJust state.budgetInfo (renderBudget state)
        ]
    ]

renderBudget :: forall m. MonadAff m => State -> API.BudgetInfo -> H.ComponentHTML Action Slots m
renderBudget state info =
  HH.div []
    [ renderSummary info
    , HH.section [ classes' "section" ]
        [ HH.slot
            _transactionsTable
            unit
            TransactionsTable.component
            { transactions: info.transactions
            , isAdmin: state.isAdmin
            , allTags: state.allTags
            }
            HandleTransactionsUpdated
        ]
    ]

renderSummary :: forall w i. API.BudgetInfo -> HH.HTML w i
renderSummary info =
  HH.section [ classes' "section" ]
    [ HH.div [ classes' "box" ]
        [ HH.div [ classes' "level" ]
            [ renderStat "Monthly Limit"         (Utils.centsToEuros info.monthlyLimitCents)        Nothing
            , renderStat "Projected Limit Today" (Utils.centsToEuros info.projectedLimitTodayCents) Nothing
            , renderStat "Over / Under Today"    (overUnderStr info.overUnderTodayCents)            (Just $ overUnderClass info.overUnderTodayCents)
            ]
        ]
    ]

renderStat :: forall w i. String -> String -> Maybe String -> HH.HTML w i
renderStat label value extraClass =
  HH.div [ classes' "level-item has-text-centered" ]
    [ HH.div []
        [ HH.p [ classes' "heading" ] [ HH.text label ]
        , HH.p [ classes' $ "title is-4" <> foldMap (" " <> _) extraClass ]
            [ HH.text value ]
        ]
    ]

overUnderStr :: Int -> String
overUnderStr cents
  | cents < 0  = Utils.centsToEuros cents
  | cents == 0 = "0.00€"
  | otherwise  = "+" <> Utils.centsToEuros cents

overUnderClass :: Int -> String
overUnderClass cents
  | cents < 0  = "has-text-success"
  | cents > 0  = "has-text-danger"
  | otherwise  = ""

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Void m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    budgetInfo <- H.liftAff API.getBudget
    H.modify_ _ { budgetInfo = Just budgetInfo, loading = false }

  HandleTransactionsUpdated TransactionsTable.TransactionsUpdated -> do
    budgetInfo <- H.liftAff API.getBudget
    H.modify_ _ { budgetInfo = Just budgetInfo }
