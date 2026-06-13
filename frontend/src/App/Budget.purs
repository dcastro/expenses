module App.Budget where

import Prelude

import App.TransactionsTable as TransactionsTable
import Charts.BudgetCharts as BudgetCharts
import Core.API as API
import Core.APITypes (TagGroupName, TagName)
import Core.APITypes as API
import Core.YearMonth (YearMonth)
import Core.YearMonth as YM
import Data.Array as Arr
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import Effect.Aff.Class (class MonadAff)
import Effect.Now as Now
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import Halogen.Subscription as HS
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Type.Proxy (Proxy(..))
import Utils (whenJust)
import Utils as Utils
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type Slot id = forall query. H.Slot query Void id

type Slots =
  ( transactionsTable :: TransactionsTable.Slot Unit
  )

_transactionsTable = Proxy :: Proxy "transactionsTable"

type Input =
  { isAdmin :: Boolean
  , allTags :: Array TagName
  , minMonth :: YearMonth -- ^ The oldest month the user can select.
  }

type Output = Void

type State =
  { isAdmin :: Boolean
  , allTags :: Array TagName
  , minMonth :: YearMonth -- ^ The oldest month the user can select.
  , budgetInfo :: Maybe API.BudgetInfo
  , loading :: Boolean
  , chart :: Maybe Foreign
  , selectedTagGroup :: Maybe TagGroupName
  -- These two are `Nothing` until `Initialize` figures out the current month.
  , selectedMonth :: Maybe YearMonth
  , maxMonth :: Maybe YearMonth -- ^ The current month; the most recent month the user can select.
  }

data Action
  = Initialize
  | TagGroupSelected (Maybe TagGroupName)
  | MonthSelectionChanged String
  | HandleTransactionsUpdated TransactionsTable.Output
  | HandleKey H.SubscriptionId KE.KeyboardEvent

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \{ isAdmin, allTags, minMonth } ->
        { isAdmin
        , allTags
        , minMonth
        , budgetInfo: Nothing
        , loading: false
        , chart: Nothing
        , selectedTagGroup: Nothing
        , selectedMonth: Nothing
        , maxMonth: Nothing
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
        , renderMonthPicker state
        , if state.loading then
            HH.p [ classes' "has-text-grey has-text-centered mt-4" ] [ HH.text "Loading..." ]
          else
            HtmlUtils.displayWhenJust state.budgetInfo (renderSummary state)
        , HH.div
            [ HP.id "budget-chart-container"
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

renderMonthPicker :: forall m. State -> H.ComponentHTML Action Slots m
renderMonthPicker state =
  case state.selectedMonth, state.maxMonth of
    Just selectedMonth, Just maxMonth ->
      HH.div [ classes' "columns is-centered" ]
        [ HH.div [ classes' "column is-narrow" ]
            [ HH.div [ classes' "field" ]
                [ HH.div [ classes' "control" ]
                    [ HtmlUtils.input'
                        [ HP.type_ InputMonth
                        , HP.value $ YM.formatYearMonth selectedMonth
                        , HtmlUtils.minYearMonth state.minMonth
                        , HtmlUtils.maxYearMonth maxMonth
                        , classes' "input"
                        , HE.onValueChange MonthSelectionChanged
                        ]
                    ]
                ]
            ]
        ]
    _, _ -> HH.text ""

renderSummary :: forall m. State -> API.BudgetInfo -> H.ComponentHTML Action Slots m
renderSummary _state info =
  HH.div [ classes' "box mt-4" ]
    [ HH.div [ classes' "level" ]
        [ renderStat
            "Monthly Limit"
            (Utils.centsToEuros info.monthlyLimitCents)
            Nothing
        , renderStat
            "Spent"
            (Utils.centsToEuros info.actualSpendingToDateCents)
            Nothing
        , renderStat
            "Remaining"
            (Utils.centsToEuros info.remainingCents)
            (Just $ remainingClass info.remainingCents)
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

remainingClass :: Int -> String
remainingClass cents
  | cents > 0 = "has-text-success"
  | cents < 0 = "has-text-danger"
  | otherwise = ""

filteredTransactions :: State -> Array API.TransactionItem
filteredTransactions state =
  case state.selectedTagGroup, state.budgetInfo of
    _, Nothing -> []
    Nothing, Just info -> info.transactions
    Just groupName, Just info ->
      case Map.lookup groupName info.tagGroupStats of
        Nothing -> info.transactions
        Just group -> Arr.filter (\tx -> tx.tag `Arr.elem` group.tags) info.transactions

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Void m Unit
handleAction = case _ of
  Initialize -> do
    currentMonth <- YM.dateToYearMonth <$> H.liftEffect Now.nowDate
    H.modify_ _ { loading = true, selectedMonth = Just currentMonth, maxMonth = Just currentMonth }
    budgetInfo <- H.liftAff $ API.getBudget currentMonth
    { emitter, listener } <- H.liftEffect HS.create
    chart <- H.liftEffect $ BudgetCharts.makeChart "budget-chart-container" budgetInfo.tagGroupStats
      \maybeGroup -> HS.notify listener (TagGroupSelected (Null.toMaybe maybeGroup))
    _sub <- H.subscribe emitter
    H.modify_ _ { budgetInfo = Just budgetInfo, loading = false, chart = Just chart }
    document <- H.liftEffect $ Window.document =<< HTML.window
    H.subscribe' \sid ->
      QE.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  TagGroupSelected maybeGroup ->
    H.modify_ _ { selectedTagGroup = maybeGroup }

  MonthSelectionChanged inputStr ->
    case YM.parseYearMonth inputStr of
      Nothing -> pure unit
      Just ym -> do
        H.modify_ _ { selectedMonth = Just ym }
        refreshBudget

  HandleTransactionsUpdated TransactionsTable.TransactionsUpdated ->
    refreshBudget

  HandleKey _sid ev -> do
    isTargettingInputElement <- H.liftEffect $ HtmlUtils.isInputElement (KE.toEvent ev)
    unless isTargettingInputElement do
      case KE.key ev of
        "q" -> changeMonth ev \s -> s.selectedMonth >>= \m -> YM.prevMonth m s.minMonth
        "e" -> changeMonth ev \s -> do
          m <- s.selectedMonth
          maxM <- s.maxMonth
          YM.nextMonth m maxM
        _ -> pure unit

changeMonth :: forall m. MonadAff m => KE.KeyboardEvent -> (State -> Maybe YearMonth) -> H.HalogenM State Action Slots Void m Unit
changeMonth ev getNextMonth = do
  state <- H.get
  whenJust (getNextMonth state) \ym -> do
    H.liftEffect $ E.preventDefault (KE.toEvent ev)
    H.modify_ _ { selectedMonth = Just ym }
    refreshBudget

-- Re-fetch the budget info for the selected month and refresh the chart.
refreshBudget :: forall m. MonadAff m => H.HalogenM State Action Slots Void m Unit
refreshBudget = do
  state <- H.get
  case state.selectedMonth of
    Nothing -> pure unit
    Just month -> do
      budgetInfo <- H.liftAff $ API.getBudget month
      case state.chart of
        Nothing -> pure unit
        Just chart -> H.liftEffect do
          BudgetCharts.clearSelection chart
          BudgetCharts.updateChart chart budgetInfo.tagGroupStats
      H.modify_ _ { budgetInfo = Just budgetInfo, selectedTagGroup = Nothing }
