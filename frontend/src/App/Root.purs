module App.Root where

import Prelude

import App.MonthRange as MonthRange
import App.NewTransactionModal as NewTransactionModal
import App.Routes (AppRoute(..))
import App.Routes as Routes
import App.SearchTransactionsTable as Search
import App.SingleMonth as SingleMonth
import Core.APITypes (TagName)
import Core.APITypes as API
import Core.Sorting as Sorting
import Core.YearMonth (YearMonth)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import Halogen.Router.Class (class MonadNavigate, class MonadRouter)
import Halogen.Router.Class as R
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Routing.Duplex as RouteDuplex
import Type.Proxy (Proxy(..))
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME

type Slot id = forall query. H.Slot query Output id

type Slots =
  ( singleMonth :: SingleMonth.Slot Unit
  , monthRange :: MonthRange.Slot Unit
  , search :: Search.Slot Unit
  , newTransactionModal :: NewTransactionModal.Slot Unit
  )

_singleMonth = Proxy :: Proxy "singleMonth"
_monthRange = Proxy :: Proxy "monthRange"
_search = Proxy :: Proxy "search"
_newTransactionModal = Proxy :: Proxy "newTransactionModal"

type Input =
  { maxMonth :: YearMonth
  , transactions :: API.GetTransactions
  , minMonth :: YearMonth -- ^ The oldest month the user can select.
  , isAdmin :: Boolean
  , initRoute :: Maybe AppRoute

  , allTags :: Array TagName
  , allAccounts :: Array String
  }

type Output = Void

type State =
  { currentRoute :: AppRoute
  , navbarActive :: Boolean

  -- These fields are used solely to initialize the `SingleMonth` and `MonthRange` components.
  -- There's no point in modifying these fields after initialization, they're never used again.
  , maxMonth :: YearMonth
  , transactions :: API.GetTransactions
  , minMonth :: YearMonth -- ^ The oldest month the user can select.
  , isAdmin :: Boolean

  , allTags :: Array TagName
  , allAccounts :: Array String

  -- Cached search parameters to preserve state when navigating away from the search route.
  , cachedSearchRoute :: Routes.SearchRoute
  }

data Action
  = Initialize
  | RouteChanged AppRoute
  | ToggleNavbarActive
  | HandleKey H.SubscriptionId KE.KeyboardEvent
  | HandleNavClick AppRoute ME.MouseEvent
  | NavigateTo AppRoute
  | HandleNewTransactionModal NewTransactionModal.Output

component :: forall q m. MonadAff m => MonadRouter AppRoute m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \{ maxMonth, transactions, minMonth, isAdmin, initRoute, allTags, allAccounts } -> do
        let
          initSearchRoute = case initRoute of
            Just (Search { searchRoute }) -> searchRoute
            _ -> defaultSearchRoute

          initialRoute = initRoute # fromMaybe Routes.defaultSingleMonthRoute

        { maxMonth
        , transactions
        , minMonth
        , isAdmin
        , currentRoute: initialRoute
        , navbarActive: false
        , cachedSearchRoute: initSearchRoute
        , allTags
        , allAccounts
        }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  defaultSearchRoute =
    { tag: Nothing
    , account: Nothing
    , allFields: ""
    , date: ""
    , desc: ""
    , amount: ""
    , notes: ""
    , isExpense: Nothing
    , column: Sorting.defaultSorted.column
    , order: Sorting.defaultSorted.order
    }

render :: forall m. MonadAff m => MonadNavigate AppRoute m => State -> H.ComponentHTML Action Slots m
render state =
  let
    singleMonthRoute = Routes.defaultSingleMonthRoute
    monthRangeRoute = Routes.defaultMonthRangeRoute
    searchRoute = Routes.mkSearchAppRoute state.cachedSearchRoute false -- Navigate to the Search route, preserving the last tag params.
    newTransactionRoute = Routes.setModalOpen true state.currentRoute
    routeToHref route = "#" <> RouteDuplex.print Routes.routeCodec route
  in
    HH.div
      []
      [ HH.nav [ classes' "navbar is-spaced has-shadow" ]
          [ HH.div [ classes' "navbar-brand" ]
              [ HH.a [ classes' "navbar-item" ]
                  [ HH.h4 [ classes' "title is-4" ] [ HH.text "€ Expenses Manager" ] ]
              , HH.a
                  [ classes' $ "navbar-burger" # HtmlUtils.addClassIf state.navbarActive "is-active"
                  , HE.onClick \_ -> ToggleNavbarActive
                  ]
                  -- These 4 empty spans are used to display the burger / cross icons
                  -- https://bulma.io/documentation/components/navbar/#navbar-burger
                  [ HH.span [] []
                  , HH.span [] []
                  , HH.span [] []
                  , HH.span [] []
                  ]

              ]
          , HH.div [ classes' $ "navbar-menu" # HtmlUtils.addClassIf state.navbarActive "is-active" ]
              ( [ HH.div [ classes' "navbar-start" ]
                    [ HH.a
                        [ classes' $ "navbar-item is-tab" # HtmlUtils.addClassIf (Routes.isSingleMonth state.currentRoute) "is-active"
                        , HP.href (routeToHref singleMonthRoute)
                        , HE.onClick (HandleNavClick singleMonthRoute)
                        ]
                        [ HH.text "Single month" ]
                    , HH.a
                        [ classes' $ "navbar-item is-tab" # HtmlUtils.addClassIf (Routes.isMonthRange state.currentRoute) "is-active"
                        , HP.href (routeToHref monthRangeRoute)
                        , HE.onClick (HandleNavClick monthRangeRoute)
                        ]
                        [ HH.text "Multiple months" ]
                    , HH.a
                        [ classes' $ "navbar-item is-tab" # HtmlUtils.addClassIf (Routes.isSearchRoute state.currentRoute) "is-active"
                        , HP.href (routeToHref searchRoute)
                        , HE.onClick (HandleNavClick searchRoute)
                        ]
                        [ HH.text "Search" ]
                    ]
                ]
                  <>
                    if state.isAdmin then
                      [ HH.div [ classes' "navbar-end" ]
                          [ HH.div [ classes' "navbar-item" ]
                              [ HH.a
                                  [ classes' "button is-primary"
                                  , HP.href (routeToHref newTransactionRoute)
                                  , HE.onClick (HandleNavClick newTransactionRoute)
                                  ]
                                  [ HH.span [ classes' "icon" ]
                                      [ HH.i [ classes' "fas fa-plus" ] [] ]
                                  , HH.span [] [ HH.text "New transaction" ]
                                  ]
                              ]
                          ]
                      ]
                    else
                      []
              )
          ]

      , HH.slot_
          _singleMonth
          unit
          SingleMonth.component
          { transactions: state.transactions
          , minMonth: state.minMonth
          , maxMonth: state.maxMonth
          , enabled: Routes.isSingleMonth state.currentRoute
          , isAdmin: state.isAdmin
          , allTags: state.allTags
          }
      , HH.slot_
          _monthRange
          unit
          MonthRange.component
          { transactions: state.transactions
          , minMonth: state.minMonth
          , maxMonth: state.maxMonth
          , enabled: Routes.isMonthRange state.currentRoute
          , isAdmin: state.isAdmin
          , allTags: state.allTags
          }
      , HH.slot_
          _search
          unit
          Search.component
          { enabled: Routes.isSearchRoute state.currentRoute
          -- I gotta keep passing the same params, even when the Search route is not enabled,
          -- otherwise the component will reset its state when navigating away and back.
          , searchRoute: state.cachedSearchRoute
          , isAdmin: state.isAdmin
          , allTags: state.allTags
          , allAccounts: state.allAccounts
          }
      , HtmlUtils.displayIf (Routes.isModalOpen state.currentRoute) $
          HH.slot
            _newTransactionModal
            unit
            NewTransactionModal.component
            { allTags: state.allTags
            , allAccounts: state.allAccounts
            }
            HandleNewTransactionModal
      ]

handleAction :: forall m. MonadRouter AppRoute m => MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do

    -- Subscribe to key events
    document <- H.liftEffect $ Window.document =<< HTML.window
    H.subscribe' \sid ->
      QE.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

    -- emitMatched is provided by MonadRouter class.
    emitter <- R.emitMatched
    void $ H.subscribe $ map RouteChanged emitter

  RouteChanged route -> do
    Console.log $ "[Root] Route changed: " <> show route
    H.modify_ \s ->
      let
        baseState = s { currentRoute = route }
      in
        case Routes.getSearchRoute route of
          Just searchRoute -> baseState { cachedSearchRoute = searchRoute }
          Nothing -> baseState

  HandleKey _sid ev -> do
    -- If an `input` element is currently focused, then we don't want to handle keyboard events.
    isTargettingInputElement <- H.liftEffect $ HtmlUtils.isInputElement (KE.toEvent ev)
    unless isTargettingInputElement do
      case KE.key ev of
        "m" -> do
          H.liftEffect $ E.preventDefault (KE.toEvent ev)
          state <- H.get
          case state.currentRoute of
            SingleMonth _ -> handleAction $ NavigateTo Routes.defaultMonthRangeRoute
            MonthRange _ -> do
              -- Navigate to the Search route, preserving the last tag params.
              state <- H.get
              handleAction $ NavigateTo $ Routes.mkSearchAppRoute state.cachedSearchRoute false
            Search _ -> handleAction $ NavigateTo Routes.defaultSingleMonthRoute
        _ -> pure unit
    pure unit

  ToggleNavbarActive -> do
    H.modify_ \s -> s { navbarActive = not s.navbarActive }

  HandleNavClick route ev -> do
    -- If the user pressed ctrl/cmd/shift/alt while clicking the link, or used
    -- middle/right click, then we want the browser to handle the event (e.g.
    -- open in new tab/window). Otherwise, we handle the navigation in-app.
    if shouldHandleInApp ev then do
      H.liftEffect $ E.preventDefault (ME.toEvent ev)
      handleAction $ NavigateTo route
    else
      pure unit

  NavigateTo route -> do
    R.navigate route

  HandleNewTransactionModal output -> do
    route <- H.gets _.currentRoute
    R.navigate $ Routes.setModalOpen false route

    case output of
      NewTransactionModal.ModalClosed -> pure unit
      NewTransactionModal.TransactionCreated -> pure unit

  where
  -- | Check if a click on a navbar link was a "modified click" (e.g. ctrl+click) or a "normal click".
  shouldHandleInApp :: ME.MouseEvent -> Boolean
  shouldHandleInApp ev =
    let
      modified = ME.ctrlKey ev || ME.metaKey ev || ME.shiftKey ev || ME.altKey ev || ME.button ev /= 0
    in
      not modified
