module App.Budget where

import Prelude

import App.TransactionsTable as TransactionsTable
import Charts.Charts as Charts
import Control.Alternative (guard)
import Core.API as API
import Core.APITypes (TagName)
import Core.APITypes as API
import Data.Array as Arr
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable as Null
import Data.String (Pattern(..), take) as S
import Data.String.Common (split, trim) as SCom
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Type.Proxy (Proxy(..))
import Utils as Utils
import Web.Event.Event as E

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
  , selectedTag :: Maybe TagName
  , editingLimit :: Boolean
  , limitInput :: String
  , savingLimit :: Boolean
  }

data Action
  = Initialize
  | TagSelected (Maybe TagName)
  | HandleTransactionsUpdated TransactionsTable.Output
  | StartEditLimit
  | LimitInputChanged String
  | SaveLimit E.Event
  | CancelEditLimit

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \{ isAdmin, allTags } ->
        { isAdmin
        , allTags
        , budgetInfo: Nothing
        , loading: false
        , chart: Nothing
        , selectedTag: Nothing
        , editingLimit: false
        , limitInput: ""
        , savingLimit: false
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
        , HH.div
            [ HP.id "budget-chart-container"
            -- Make the area a little bit taller, so that the "ZoomCharts Unlicensed"
            -- red box doesn't appear above the chart's labels
            , HP.style "height: 400px"
            ]
            []
        , if state.loading then
            HH.p [ classes' "has-text-grey has-text-centered mt-4" ] [ HH.text "Loading..." ]
          else
            HtmlUtils.displayWhenJust state.budgetInfo (renderSummary state)
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
renderSummary state info =
  HH.div [ classes' "box mt-4" ]
    [ HH.div [ classes' "level" ]
        [ renderLimitStat
            state
            info
        , renderStat
            "Expected spending to date"
            (Utils.centsToEuros info.projectedLimitTodayCents)
            Nothing
        , renderStat
            "Over / Under expected spending"
            (overUnderStr info.overUnderTodayCents)
            (Just $ overUnderClass info.overUnderTodayCents)
        ]
    ]

renderLimitStat :: forall m. State -> API.BudgetInfo -> H.ComponentHTML Action Slots m
renderLimitStat state info =
  HH.div [ classes' "level-item has-text-centered" ]
    [ HH.div []
        [ HH.p [ classes' "heading" ] [ HH.text "Monthly Limit" ]
        , if state.editingLimit then
            renderLimitEditor state
          else
            HH.p [ classes' "title is-4" ]
              [ HH.text (Utils.centsToEuros info.monthlyLimitCents)
              , HtmlUtils.displayIf state.isAdmin $
                  HH.button
                    [ classes' "button is-ghost is-small ml-2"
                    , HP.title "Edit limit"
                    , HE.onClick \_ -> StartEditLimit
                    ]
                    [ HH.span [ classes' "icon is-small" ]
                        [ HH.i [ classes' "fas fa-pencil-alt" ] [] ]
                    ]
              ]
        ]
    ]

renderLimitEditor :: forall m. State -> H.ComponentHTML Action Slots m
renderLimitEditor state =
  HH.form
    [ HE.onSubmit SaveLimit
    ]
    [ HH.div [ classes' "field has-addons mt-2" ]
        [ HH.div [ classes' "control" ]
            [ HtmlUtils.input'
                [ HP.type_ InputText
                , HP.value state.limitInput
                , HP.style "width: 8rem"
                , classes' "input"
                , HE.onValueChange LimitInputChanged
                ]
            ]
        , HH.div [ classes' "control" ]
            [ HH.span [ classes' "button is-static" ]
                [ HH.text "€" ]
            ]
        , HH.div [ classes' "control" ]
            [ HH.button
                [ classes' $ "button is-success" # HtmlUtils.addClassIf state.savingLimit "is-loading"
                , HP.disabled (isNothing (parseEuros state.limitInput))
                , HP.type_ HP.ButtonSubmit
                ]
                [ HH.text "✓" ]
            ]
        , HH.div [ classes' "control" ]
            [ HH.button
                [ classes' "button is-light"
                , HE.onClick \_ -> CancelEditLimit
                ]
                [ HH.text "✗" ]
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
  | cents < 0 = Utils.centsToEuros cents
  | cents == 0 = "0.00€"
  | otherwise = "+" <> Utils.centsToEuros cents

overUnderClass :: Int -> String
overUnderClass cents
  | cents < 0 = "has-text-success"
  | cents > 0 = "has-text-danger"
  | otherwise = ""

-- | Parse a euro amount string (e.g. "500", "500.5", "500.50") to cents.
parseEuros :: String -> Maybe Int
parseEuros str = do
  case SCom.split (S.Pattern ".") (SCom.trim str) of
    [ eurosStr ] -> do
      euros <- Int.fromString eurosStr
      guard (euros > 0)
      pure (euros * 100)
    [ eurosStr, centsStr ] -> do
      euros <- Int.fromString eurosStr
      -- Pad cents to 2 digits (e.g. "5" → "50"), truncate if longer (e.g. "500" → "50")
      cents <- Int.fromString (S.take 2 (centsStr <> "0"))
      guard (euros > 0 || cents > 0)
      pure (euros * 100 + cents)
    _ -> Nothing

filteredTransactions :: State -> Array API.TransactionItem
filteredTransactions state =
  case state.selectedTag, state.budgetInfo of
    _, Nothing -> []
    Nothing, Just info -> info.transactions
    Just tag, Just info -> Arr.filter (\tx -> tx.tag == Just tag) info.transactions

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Void m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    budgetInfo <- H.liftAff API.getBudget
    { emitter, listener } <- H.liftEffect HS.create
    chart <- H.liftEffect $ Charts.makeBudgetChart "budget-chart-container" budgetInfo.tagStats
      \maybeTag -> HS.notify listener (TagSelected (Null.toMaybe maybeTag))
    _sub <- H.subscribe emitter
    H.modify_ _ { budgetInfo = Just budgetInfo, loading = false, chart = Just chart }

  TagSelected maybeTag ->
    H.modify_ _ { selectedTag = maybeTag }

  StartEditLimit -> do
    state <- H.get
    let
      currentValue = case state.budgetInfo of
        Just info -> Utils.centsToEurosRaw info.monthlyLimitCents
        Nothing -> ""
    H.modify_ _ { editingLimit = true, limitInput = currentValue }

  LimitInputChanged str ->
    H.modify_ _ { limitInput = str }

  CancelEditLimit ->
    H.modify_ _ { editingLimit = false }

  SaveLimit ev -> do
    H.liftEffect $ E.preventDefault ev

    state <- H.get
    case parseEuros state.limitInput of
      Nothing -> pure unit
      Just limitCents -> do
        H.modify_ _ { savingLimit = true }
        H.liftAff $ API.setBudgetLimit limitCents
        budgetInfo <- H.liftAff API.getBudget
        case state.chart of
          Just chart -> H.liftEffect $ Charts.updateBudgetChart chart budgetInfo.tagStats
          Nothing -> pure unit
        H.modify_ _ { budgetInfo = Just budgetInfo, editingLimit = false, savingLimit = false }

  HandleTransactionsUpdated TransactionsTable.TransactionsUpdated -> do
    budgetInfo <- H.liftAff API.getBudget
    state <- H.get
    case state.chart of
      Nothing -> pure unit
      Just chart -> do
        H.liftEffect $ Charts.clearSelection chart
        H.liftEffect $ Charts.updateBudgetChart chart budgetInfo.tagStats
    H.modify_ _ { budgetInfo = Just budgetInfo, selectedTag = Nothing }
