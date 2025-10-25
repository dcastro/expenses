module App.SingleMonth where

import Prelude

import App.TransactionsTable as TransactionsTable
import Charts.Charts as Charts
import Core.API as API
import Core.APITypes (TagGroupName, TagName)
import Core.APITypes as API
import Core.Display (display)
import Core.YearMonth (YearMonth)
import Core.YearMonth as YM
import Data.Array (elem)
import Data.Array as Arr
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Foreign (Foreign)
import Foreign as Foreign
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import Halogen.Subscription as HS
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Utils (whenJust)
import Utils as Utils
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type Slot id = forall query. H.Slot query Output id

type Slots =
  ( transactionsTable :: TransactionsTable.Slot Unit
  )

_transactionsTable = Proxy :: Proxy "transactionsTable"

type Input =
  { thisMonth :: YearMonth
  , transactions :: API.GetTransactions
  , minMonth :: YearMonth -- ^ The oldest month the user can select.
  , enabled :: Boolean
  , isAdmin :: Boolean
  , allTags :: Array TagName
  }

type Output = Void

type State =
  { chart :: Foreign
  , selectedMonth :: YearMonth
  , transactions :: API.GetTransactions
  , selection :: ChartSelection
  , minMonth :: YearMonth -- ^ The oldest month the user can select.
  , maxMonth :: YearMonth
  , enabled :: Boolean -- ^ Whether this component is currently being displayed
  , isAdmin :: Boolean
  , chartsHaveBeenInitialized :: Boolean -- ^ Whether this component's charts have ever been initialized
  , allTags :: Array TagName
  }

data Action
  = Initialize
  | Receive Input
  | ChartSelectionChanged ChartSelection
  | MonthSelectionChanged String
  | TransactionsTableUpdated TransactionsTable.Output
  | HandleKey H.SubscriptionId KE.KeyboardEvent

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState: \{ thisMonth, transactions, minMonth, enabled, isAdmin, allTags } -> do
        {
          -- TODO: get rid of this hack
          chart: Foreign.unsafeToForeign 1
        , selectedMonth: thisMonth
        , transactions
        , selection: NoSelection
        , minMonth
        , maxMonth: thisMonth
        , enabled
        , isAdmin
        , chartsHaveBeenInitialized: false
        , allTags
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.style $
        if state.enabled then
          ""
        else
          "display: none"

    ]
    [ HH.section [ classes' "section" ]
        [ HH.div [ classes' "columns is-centered box" ]
            [ HH.div [ classes' "column is-narrow" ]
                [ HH.label [ classes' "label" ] [ HH.text "Filters" ]
                , HH.div
                    [ classes' "field" ]
                    [ HH.div [ classes' "control" ]
                        [ HtmlUtils.input'
                            [ HP.type_ InputMonth
                            , HP.value $ YM.formatYearMonth state.selectedMonth
                            , HtmlUtils.minYearMonth state.minMonth
                            , HtmlUtils.maxYearMonth state.maxMonth
                            , classes' "input"
                            , HE.onValueChange MonthSelectionChanged
                            ]
                        ]
                    ]
                ]
            , HH.div [ classes' "column" ]
                [ HH.div
                    [ HP.id "single-month-chart-container"
                    -- Make the area a little bit taller, so that the "ZoomCharts Unlicensed"
                    -- red box doesn't appear above the chart's labels
                    , HP.style "height: 400px"
                    ]
                    []
                ]
            ]
        ]
    , HH.section [ classes' "section" ]
        [ HH.h4 [ classes' "title is-4 has-text-centered" ]
            [ HH.text "Transactions" ]
        , HH.div [ classes' "is-flex is-justify-content-center" ]
            [ HH.table [ classes' "table is-narrow" ]
                [ HH.tbody [] $
                    let
                      txs = state.transactions

                      displayRow label amount =
                        HH.tr []
                          [ HH.td [ classes' "has-text-left" ] [ HH.text label ]
                          , HH.td [ classes' "has-text-right" ] [ HH.text $ Utils.centsToEuros amount ]
                          ]
                    in
                      [ displayRow "All tags" txs.totalAmountCents
                      , HtmlUtils.displayWhenJust (getSelectedTagGroup state.selection) \groupName ->
                          displayRow (display groupName) (getGroupTotal txs.groupsStats groupName)
                      , HtmlUtils.displayWhenJust (getSelectedTag state.selection) \tagName ->
                          displayRow (display tagName) (getTagTotal txs.groupsStats tagName)
                      ]
                ]
            ]
        ]
    , HH.section [ classes' "section is-fullheight" ]
        [ HH.slot
            _transactionsTable
            unit
            TransactionsTable.component
            { transactions: filterTransactions state
            , isAdmin: state.isAdmin
            , allTags: state.allTags
            }
            TransactionsTableUpdated
        ]
    ]

-- If a tag / tag group is selected, filter out any transactions that don't belong.
filterTransactions :: State -> Array API.TransactionItem
filterTransactions state =
  case state.selection of
    NoSelection -> txs.transactions
    SelectedGroup groupName -> do
      let groupTags = getGroupTags groupName
      txs.transactions
        # Arr.filter
            ( \tx ->
                case tx.tag of
                  Nothing -> false
                  Just tag -> elem tag groupTags
            )
    SelectedTag _groupName tagName ->
      txs.transactions
        # Arr.filter
            ( \tx ->
                case tx.tag of
                  Nothing -> false
                  Just tag -> tag == tagName
            )
  where
  txs = state.transactions

  -- Helper to get all tags for a group
  getGroupTags :: TagGroupName -> Array TagName
  getGroupTags groupName =
    case Arr.find (\g -> g.name == groupName) txs.groupsStats of
      Just group -> group.tags <#> _.name
      Nothing -> []

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    tryInitializeCharts

    -- Subscribe to key events
    document <- H.liftEffect $ Window.document =<< HTML.window
    H.subscribe' \sid ->
      QE.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
    pure unit

  Receive input -> do
    H.modify_ _ { enabled = input.enabled }
    tryInitializeCharts

  ChartSelectionChanged chartSelection -> do
    H.liftEffect $ Console.log $ "SELECTION: " <> show chartSelection
    H.modify_ \s -> s { selection = chartSelection }

  MonthSelectionChanged inputStr -> do
    case YM.parseYearMonth inputStr of
      Nothing -> pure unit
      Just ym -> do
        Console.log $ "Changed month to: " <> show ym
        H.modify_ \s -> s { selectedMonth = ym }
        downloadTransactionsAndUpdateChart

  TransactionsTableUpdated output -> do
    case output of
      TransactionsTable.TransactionsUpdated -> do
        Console.log "[SingleMonth] Child table updated; refreshing transactions"
        -- We re-fetch the data data after every edit to keep charts and summary stats consistent.
        downloadTransactionsAndUpdateChart

  HandleKey _sid ev -> do
    state <- H.get
    when state.enabled do
      -- If an `input` element is currently focused, then we don't want to handle keyboard events.
      isTargettingInputElement <- H.liftEffect $ HtmlUtils.isInputElement (KE.toEvent ev)
      unless isTargettingInputElement do
        case KE.key ev of
          -- Select previous month
          "ArrowLeft" -> do
            changeSingleMonth ev \s -> YM.prevMonth s.selectedMonth s.minMonth
          "ArrowRight" -> do
            changeSingleMonth ev \s -> YM.nextMonth s.selectedMonth s.maxMonth
          "q" -> do
            changeSingleMonth ev \s -> YM.prevMonth s.selectedMonth s.minMonth
          "e" -> do
            changeSingleMonth ev \s -> YM.nextMonth s.selectedMonth s.maxMonth
          _ -> pure unit

tryInitializeCharts :: forall o m. MonadEffect m => H.HalogenM State Action Slots o m Unit
tryInitializeCharts = do
  state <- H.get
  -- NOTE: we initialize the charts ONLY after the component has been rendered on the screen.
  -- Otherwise, zoomcharts will not correctly calculate the chart's width/height.
  -- For that reason, this function can only be called after running `H.put` (to force a render) with `enabled = true`
  when (state.enabled && not (state.chartsHaveBeenInitialized)) do
    Console.log "Initializing charts for SingleMonth"
    { emitter, listener } <- H.liftEffect HS.create

    -- NOTE: we have to run `put` first, to force the `chart-container` to be rendered,
    -- and then we can initialize it.
    let monthSpan = 1
    chart <- H.liftEffect $ Charts.makeChart "single-month-chart-container" state.transactions monthSpan
      -- This callback is called when the user drills-down into a tag group, or when they go back up.
      ( \tagGroupName -> do
          HS.notify listener $ ChartSelectionChanged $
            case Null.toMaybe tagGroupName of
              Just tagGroupName -> SelectedGroup tagGroupName
              Nothing -> NoSelection
      )
      -- This callback is called when the user selects a slice when no subvalues.
      ( \tagGroupName tagName -> do
          HS.notify listener $ ChartSelectionChanged $
            case Null.toMaybe tagGroupName, Null.toMaybe tagName of
              Just tagGroupName, Just tagName -> SelectedTag tagGroupName tagName
              Just tagGroupName, Nothing -> SelectedGroup tagGroupName
              Nothing, Nothing -> NoSelection
              Nothing, Just _ -> unsafeCrashWith "impossible"
      )

    _subscriptionId <- H.subscribe emitter

    H.put $ state
      { chart = chart
      , chartsHaveBeenInitialized = true
      }

changeSingleMonth :: forall o m. MonadAff m => KE.KeyboardEvent -> (State -> Maybe YearMonth) -> HalogenM State Action Slots o m Unit
changeSingleMonth ev changeMonth = do
  state <- H.get
  whenJust (changeMonth state) \ym -> do
    H.liftEffect $ E.preventDefault (KE.toEvent ev)
    Console.log $ "Changed month to: " <> show ym
    H.put $ state { selectedMonth = ym }
    downloadTransactionsAndUpdateChart

downloadTransactionsAndUpdateChart :: forall o m. MonadAff m => HalogenM State Action Slots o m Unit
downloadTransactionsAndUpdateChart = do
  state <- H.get

  txs <- H.liftAff $ API.getTransactions
    state.selectedMonth
    state.selectedMonth

  let state' = state { transactions = txs }

  -- NOTE: we use `updateChart'` instead of `updateChart` because we need to apply all modifications to state before
  -- calling `H.put` and cause a re-rendering.
  --
  -- More specifically, we need to modify the `transactions` field and the `selection` field in the same "state transaction",
  -- otherwise the rendering function will attempt to display data for a tag group / tag that was previously selected
  -- but does not exist in the new batch of transactions, causing a runtime error.
  state'' <- H.liftEffect $ updateChart' state'
  H.put state''

updateChart' :: State -> Effect State
updateChart' state = do
  let txs = state.transactions

  -- NOTE: the charts library bugs out if you swap the chart data and the slice that was selected before
  -- no longer exists in the new data.
  -- So we need to undo the selection, update the chart, then reapply the selection (if it still exists).
  let previousSelection = state.selection
  newSelection <- H.liftEffect do
    let monthSpan = 1
    Charts.clearSelection state.chart
    Charts.updateChart state.chart txs monthSpan
    -- If the selection is still valid, re-select in the pie chart
    -- If it's no longer valid, update `LoadedState`'s selection to NoSelection
    case previousSelection of
      NoSelection -> pure previousSelection
      SelectedGroup groupName ->
        case Arr.find (\g -> g.name == groupName) txs.groupsStats of
          Nothing -> do
            pure NoSelection
          Just group -> do
            if Arr.length group.tags == 1 then do
              Charts.selectSlice state.chart (API.getTagGroupName groupName)
            else do
              Charts.drilldown state.chart groupName
            pure previousSelection
      SelectedTag groupName tagName ->
        if Arr.any (\g -> g.name == groupName && elem tagName (g.tags <#> _.name)) txs.groupsStats then do
          Charts.drilldown state.chart groupName
          Charts.selectSlice state.chart (API.getTagName tagName)
          pure previousSelection
        else do
          pure NoSelection

  H.liftEffect $ Console.log $ "NEW SELECTION: " <> show newSelection

  pure $ state { selection = newSelection }

data ChartSelection
  = NoSelection
  | SelectedGroup TagGroupName
  | SelectedTag TagGroupName TagName

derive instance Generic ChartSelection _

instance Show ChartSelection where
  show = genericShow

getSelectedTagGroup :: ChartSelection -> Maybe TagGroupName
getSelectedTagGroup = case _ of
  NoSelection -> Nothing
  SelectedGroup groupName -> Just groupName
  SelectedTag groupName _ -> Just groupName

getSelectedTag :: ChartSelection -> Maybe TagName
getSelectedTag = case _ of
  NoSelection -> Nothing
  SelectedGroup _ -> Nothing
  SelectedTag _ tagName -> Just tagName

getGroupTotal :: Array API.TagGroupStats -> TagGroupName -> Int
getGroupTotal stats groupName =
  case Arr.find (\g -> g.name == groupName) stats of
    Just group -> group.groupTotalAmountCents
    Nothing -> unsafeCrashWith $ "Failed to find total for tag group: " <> show groupName

getTagTotal :: Array API.TagGroupStats -> TagName -> Int
getTagTotal stats tagName =
  case Arr.findMap (\g -> Arr.find (\t -> t.name == tagName) g.tags) stats of
    Just tag -> tag.tagTotalAmountCents
    Nothing -> unsafeCrashWith $ "Failed to find total for tag: " <> show tagName
