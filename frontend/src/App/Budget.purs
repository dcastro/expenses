module App.Budget where

import Prelude

import App.TransactionsTable as TransactionsTable
import Charts.BudgetCharts as BudgetCharts
import Core.API as API
import Core.APITypes (TagGroupName, TagName)
import Core.APITypes as API
import Data.Array as Arr
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable as Null
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
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
  , chart :: Maybe Foreign
  , selectedTagGroup :: Maybe TagGroupName
  }

data Action
  = Initialize
  | TagGroupSelected (Maybe TagGroupName)
  | HandleTransactionsUpdated TransactionsTable.Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \{ isAdmin, allTags } ->
        { isAdmin
        , allTags
        , budgetInfo: Nothing
        , loading: false
        , chart: Nothing
        , selectedTagGroup: Nothing
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
            [ HH.text "Budget" ]
        , if state.loading then
            HH.p [ classes' "has-text-grey has-text-centered mt-4" ] [ HH.text "Loading..." ]
          else
            HtmlUtils.displayWhenJust state.budgetInfo (renderSummary state)
        , HH.div
            [ HP.id "budget-chart-container"
            -- Make the area a little bit taller, so that the "ZoomCharts Unlicensed"
            -- red box doesn't appear above the chart's labels
            , HP.style "height: 400px"
            ]
            []
        ]
    , HH.section [ classes' "section is-fullheight" ]
        [ HH.slot
            _transactionsTable
            unit
            TransactionsTable.component
            { transactions: filteredTransactions state
            , isAdmin: state.isAdmin
            , allTags: state.allTags
            }
            HandleTransactionsUpdated
        ]
    ]

renderSummary :: forall m. State -> API.BudgetInfo -> H.ComponentHTML Action Slots m
renderSummary _state info =
  HH.div [ classes' "box mt-4" ]
    [ HH.div [ classes' "level" ]
        [ renderStat
            "Monthly Limit"
            (Utils.centsToEuros info.monthlyLimitCents)
            Nothing
        , renderStat
            "Expected spending to date"
            (Utils.centsToEuros info.projectedLimitTodayCents)
            Nothing
        , renderStat
            "Actual spending to date"
            (Utils.centsToEuros info.totalSpentCents)
            Nothing
        , renderStat
            "Over / Under expected spending"
            (overUnderStr info.overUnderTodayCents)
            (Just $ overUnderClass info.overUnderTodayCents)
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
  | cents > 0 = "+" <> Utils.centsToEuros cents
  | otherwise = Utils.centsToEuros cents

overUnderClass :: Int -> String
overUnderClass cents
  | cents < 0 = "has-text-success"
  | cents > 0 = "has-text-danger"
  | otherwise = ""

filteredTransactions :: State -> Array API.TransactionItem
filteredTransactions state =
  case state.selectedTagGroup, state.budgetInfo of
    _, Nothing -> []
    Nothing, Just info -> info.transactions
    Just groupName, Just info ->
      let
        mGroup = Arr.find (\g -> g.name == groupName) info.tagGroupStats
      in
        case mGroup of
          Nothing -> info.transactions
          Just group -> Arr.filter (\tx -> maybe false (_ `Arr.elem` group.tags) tx.tag) info.transactions

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Void m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    budgetInfo <- H.liftAff API.getBudget
    { emitter, listener } <- H.liftEffect HS.create
    chart <- H.liftEffect $ BudgetCharts.makeChart "budget-chart-container" budgetInfo.tagGroupStats
      \maybeGroup -> HS.notify listener (TagGroupSelected (Null.toMaybe maybeGroup))
    _sub <- H.subscribe emitter
    H.modify_ _ { budgetInfo = Just budgetInfo, loading = false, chart = Just chart }

  TagGroupSelected maybeGroup ->
    H.modify_ _ { selectedTagGroup = maybeGroup }

  HandleTransactionsUpdated TransactionsTable.TransactionsUpdated -> do
    budgetInfo <- H.liftAff API.getBudget
    state <- H.get
    case state.chart of
      Nothing -> pure unit
      Just chart -> do
        H.liftEffect $ BudgetCharts.clearSelection chart
        H.liftEffect $ BudgetCharts.updateChart chart budgetInfo.tagGroupStats
    H.modify_ _ { budgetInfo = Just budgetInfo, selectedTagGroup = Nothing }
