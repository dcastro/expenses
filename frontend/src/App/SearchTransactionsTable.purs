module App.SearchTransactionsTable where

import Prelude

import App.Routes (AppRoute)
import App.Routes as Routes
import App.SelectTag as SelectTag
import App.SplitTransactionModal as SplitTransactionModal
import Choices as Choices
import Core.API as API
import Core.APITypes (RawSearchParams, TagName, TagParams(..), TransactionId, TransactionItem, getJDate)
import Core.APITypes as API
import Core.Display (display)
import Core.Sorting (Sorted, SortedOrder(..), SortingColumn(..))
import Core.Sorting as Sorting
import Core.YearMonth as YM
import Data.Array as Arr
import Data.Array.NonEmpty as NE
import Data.Foldable (sum)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Ord.Down (Down(..))
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Foreign (Foreign)
import Foreign as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import Halogen.Router.Class (class MonadNavigate)
import Halogen.Router.Class as R
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Inflection as Inflect
import Promise.Aff as JsPromise
import Record as Record
import Type.Prelude (Proxy(..))
import Utils (whenJust)
import Utils as Utils
import Web.Clipboard as Clip
import Web.DOM.Node as DOMNode
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.FocusEvent as FocusEvent
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET

{-

Handling of the `Search` route is more complicated than the other routes.
These are the ways in which we can interact with the Search route:

* When the user opens `/search?tag=gifts` in their browser
  * The search params are read in `Main`, and passed down to `Root` and then trigger `Initialize` in `SearchTransactionsTable`
    * Calls `updateControls`
    * Calls `doSearch`, unconditionally

* When the user clicks on the "Search" button in the navbar
  * Every time the search params are set/change, the `Root` component saves them in `State.cachedSearchParams`.
    This ensures the params don't change when the user navigates away and then back to the search page.
  * When the user clicks the "Search" button in the navbar, we trigger `NavigateToSearch`
  * Calls `R.navigate`
  * Triggers `RouteChanged`, updates the state's `currentRoute` and `cachedSearchParams`
  * Triggers `Receive`
    * updates the state's `searchParams.tag` (no-op, because it hasn't changed)
    * if oldtag =/= newtag, calls `doSearch` to run a search and update the table (no-op, because it hasn't changed)
    * `updateControls` (no-op, because the state matches the dropdown's selection)

* When the user opens `/` and then manually appends `?tag=gifts` to the URL in the address bar.
  * Triggers `RouteChanged`, updates the state's `currentRoute` and `cachedSearchParams`
  * Triggers `Receive`
    * updates the state's `searchParams.tag`
    * if oldtag =/= newtag, calls `doSearch` to run a search and update the table
    * `updateControls`

* When the user selects/deselects a tag from the dropdown
  * Triggers `SelectTag` event
  * Calls `R.navigate`
  * Triggers `RouteChanged`, updates the state's `currentRoute` and `cachedSearchParams`
  * Triggers `Receive`
    * updates the state's `searchParams.tag`
    * if oldtag =/= newtag, calls `doSearch` to run a search and update the table
    * `updateControls` (no-op, because the state matches the dropdown's selection)


In summary, these things need to be kept in sync with each other.
If you change one, we must change them all.
* The query parameters in the URL
* The transactions table
* The dropdown's selection
* Search's `State.searchParams`
* Root's `State.cachedSearchParams`
* Root's `State.currentRoute`

-}

type Slots =
  ( selectTag :: SelectTag.Slot Unit
  , splitTransactionModal :: SplitTransactionModal.Slot Unit
  )

_selectTag = Proxy :: Proxy "selectTag"
_splitTransactionModal = Proxy :: Proxy "splitTransactionModal"

type Slot id = forall q. H.Slot q Output id

type Output = Void

type Input =
  { enabled :: Boolean
  , searchRoute :: Routes.SearchRoute
  , isAdmin :: Boolean

  , allTags :: Array TagName
  , allAccounts :: Array String
  }

type State =
  { enabled :: Boolean
  -- NOTE: we store txs in an "array of arrays" so we can group transactions that we want to display as part of one logical group.
  -- E.g., when sorting by date, transactions that occur in the same month/year will be displayed in the same group,
  -- with a thicker border separating the groups.
  , transactions :: Array (Array TransactionItem)
  , sorted :: Sorted
  -- Details about the transaction whose menu is open.
  , menuOpened :: Maybe OpenMenuState
  , searchParams :: RawSearchParams
  , allTags :: Array TagName
  , allAccounts :: Array String
  , tagChoicesControl :: Foreign
  , accountChoicesControl :: Foreign
  , isAdmin :: Boolean

  -- Determines whether we're currently displaying a modal for selecting the tag for a transaction
  , isSelectingTagFor :: Maybe TransactionItem
  , splitModal :: Maybe SplitModalState
  }

type OpenMenuState =
  { -- | The ID of the transaction whose dropdown menu has been opened.
    transactionId :: TransactionId
  , itemIndex :: Int

  -- | The ID of the subscription for to "click" events to close the dropdown menu.
  , subscriptionId :: H.SubscriptionId
  }

type SplitModalState =
  { transactionId :: TransactionId
  , items :: Array API.ShortTransactionItem
  , totalAmountCents :: Int
  }

data Action
  = SelectedSortColumn SortingColumn
  | Initialize
  | Receive Input
  | OpenMenu ME.MouseEvent { transactionId :: TransactionId, itemIndex :: Int }
  | CloseMenu
  | CopyIdToClipboard TransactionId
  | ModifySearchQuery (RawSearchParams -> RawSearchParams)
  | SelectAccount Int
  | SelectTag Int
  | SetTransactionIsExpense TransactionItem Boolean
  | OpenSelectTagModal TransactionItem
  | HandleSelectTagOutput TransactionItem SelectTag.Output
  | SetTransactionDetails TransactionItem FocusEvent.FocusEvent
  | OpenSplitTransaction TransactionItem
  | HandleSplitModal SplitTransactionModal.Output

component :: forall q m. MonadAff m => MonadNavigate AppRoute m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: initState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

initState :: Input -> State
initState { enabled, searchRoute, isAdmin, allTags, allAccounts } =
  { transactions: []
  , sorted: searchRouteSorted searchRoute
  , menuOpened: Nothing
  , enabled
  , searchParams: searchRouteParams searchRoute
  , allTags
  , allAccounts
  -- TODO: get rid of this hack
  , tagChoicesControl: Foreign.unsafeToForeign 1
  , accountChoicesControl: Foreign.unsafeToForeign 1
  , isAdmin
  , isSelectingTagFor: Nothing
  , splitModal: Nothing
  }

sortTransactions :: Array TransactionItem -> Sorted -> Array (Array TransactionItem)
sortTransactions transactions { column, order } =
  case column of
    Date ->
      ( case order of
          Asc -> Arr.sortWith (\tx -> tx.date /\ tx.transactionId /\ tx.itemIndex) transactions
          Desc -> Arr.sortWith (\tx -> Down $ tx.date /\ tx.transactionId /\ tx.itemIndex) transactions
      )
        -- Group by YearMonth
        # Arr.groupBy ((==) `on` (\tx -> tx.date # getJDate # YM.dateToYearMonth))
        <#> NE.toArray
    Tag ->
      case order of
        Asc -> [ Arr.sortWith (\tx -> tx.tag /\ tx.transactionId /\ tx.itemIndex) transactions ]
        Desc -> [ Arr.sortWith (\tx -> Down $ tx.tag /\ tx.transactionId /\ tx.itemIndex) transactions ]
    Amount ->
      case order of
        Asc -> [ Arr.sortWith (\tx -> tx.itemAmountCents /\ tx.transactionId /\ tx.itemIndex) transactions ]
        Desc -> [ Arr.sortWith (\tx -> Down $ tx.itemAmountCents /\ tx.transactionId /\ tx.itemIndex) transactions ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.section
    [ classes' ""
    , HP.style $
        if state.enabled then
          ""
        else
          "display: none"
    ]
    [ HH.section [ classes' "section is-fullheight" ]
        [ HH.h4 [ classes' "title is-4 has-text-centered" ]
            [ HH.text "Transactions" ]
        , HH.div [ classes' "columns is-centered" ]
            [ HH.div [ classes' "column is-three-fifths-desktop" ]
                [ HH.div [ classes' "field" ]
                    [ HH.p [ classes' "control has-icons-left" ]
                        [ HH.input
                            [ classes' "input"
                            , HP.type_ InputSearch
                            , HP.placeholder "Search..."
                            , HE.onValueChange \str -> ModifySearchQuery (\p -> p { allFields = str })
                            , HP.value state.searchParams.allFields
                            ]
                        , HH.span [ classes' "icon is-left is-small" ]
                            [ HH.i [ classes' "fas fa-search" ] [] ]
                        ]
                    ]
                ]
            ]
        , HH.div [ classes' "block has-text-centered", HP.style "" ]
            let
              count = state.transactions <#> Arr.length # sum
              amountCents = state.transactions # join <#> _.itemAmountCents # sum
            in
              [ HH.p_ [ HH.text $ "Found " <> show count <> " " <> Inflect.inflect "transaction" count ]
              , HH.p_ [ HH.text $ "Total amount: " <> Utils.centsToEuros amountCents ]
              ]

        , HH.div
            [ classes' "table-container"
            -- NOTE: this a terrible hack.
            -- We need `table-container` to make the table scrollable horizontally on mobile.
            -- However, that sets `overflow-x: auto; overflow-y: hidden`, which
            -- means child elements will be also be clipped.
            -- I.e., the a row's "menu" and the tag's `select` box might be clipped.
            -- To prevent that, I'm adding a huge padding area at the bottom, so the menu and the select box can
            -- be displayed in full.
            -- Related:
            --    * https://github.com/CreativeBulma/bulma-tooltip/issues/2
            --    * https://github.com/CreativeBulma/bulma-tooltip/issues/26
            --    * https://github.com/tailwindlabs/tailwindcss/discussions/2672
            , HP.style "padding-bottom: 500px;"
            ]
            [ HH.table [ classes' "table is-hoverable is-fullwidth is-family-monospace is-bordered" ]
                [ HH.thead []
                    [ HH.tr []

                        {-
                          Requirements:
                            * When the user changes the search parameters, or switches the month selection, the table's contents will change.
                              When that happens, we don't want the columns to be resized; the layout change is visually "awkward".
                            * The "notes" column should take as much width as possible. There's a lower bound,
                              so that the column take exactly 500px on mobile.

                          Notes:
                            * We don't use `white-space: nowrap`; this implies columns could expand to accomodate more content. But according to the requirements above, they can't grow or shrink.
                            * We don't use `text-overflow: ellipsis`, because that would hide text from the user.
                            * So we have to let any overflow text simply wrap around.

                            * We don't use `width`, because "width" is interpreted as a suggestion, and will be smaller when the column's content is small.
                            * We don't need to use `max-width` on any column, because the "Notes" column will take up all the remaining space,
                              leaving no chance for the other columns to grow beyond their `min-width`.

                            * Because these rules cause the widths to be fixed, then we don't need to add these styles to each and every `td` cell.
                              Just adding them to the headers is enough.
                        -}

                        [ renderTableHeader state "Date" (Just Date) "min-width: 130px;"
                        , renderTableHeader state "Transaction" Nothing "min-width: 500px;"
                        , renderTableHeader state "Amount" (Just Amount) "min-width: 130px;"
                        , renderTableHeader state "Tag" (Just Tag) "min-width: 200px;"
                        , renderTableHeader state "Notes" Nothing "min-width: 500px; width: 100%;"
                        , renderTableHeader state "Account" Nothing "min-width: 150px;"
                        , renderTableHeader state "Is expense?" Nothing "min-width: 150px;"
                        , renderTableHeader state "" Nothing "min-width: 50px;"
                        ]
                    ]
                , HH.thead []
                    [ HH.tr []
                        [ HH.th []
                            [ HH.input
                                [ classes' "input"
                                , HP.type_ InputSearch
                                , HP.placeholder "Search..."
                                , HE.onValueChange \str -> ModifySearchQuery (\p -> p { date = str })
                                , HP.value state.searchParams.date
                                ]
                            ]
                        , HH.th []
                            [ HH.input
                                [ classes' "input"
                                , HP.type_ InputSearch
                                , HP.placeholder "Search..."
                                , HE.onValueChange \str -> ModifySearchQuery (\p -> p { desc = str })
                                , HP.value state.searchParams.desc
                                ]
                            ]
                        , HH.th []
                            [ HH.input
                                [ classes' "input"
                                , HP.type_ InputSearch
                                , HP.placeholder "Search..."
                                , HE.onValueChange \str -> ModifySearchQuery (\p -> p { amount = str })
                                , HP.value state.searchParams.amount
                                ]
                            ]
                        , HH.th []
                            [ HH.select
                                [ HP.id "tag-choices"
                                , HE.onSelectedIndexChange \idx -> SelectTag idx
                                ] $
                                [ HH.option_ [ HH.text "No tag" ]
                                ]
                                  <>
                                    ( state.allTags <#> \tag -> HH.option [ HP.value $ display tag ] [ HH.text $ display tag ]
                                    )
                            ]
                        , HH.th []
                            [ HH.input
                                [ classes' "input"
                                , HP.type_ InputSearch
                                , HP.placeholder "Search..."
                                , HE.onValueChange \str -> ModifySearchQuery (\p -> p { notes = str })
                                , HP.value state.searchParams.notes
                                ]
                            ]
                        , HH.th []
                            [ HH.select
                                [ HP.id "account-choices"
                                , HE.onSelectedIndexChange \idx -> SelectAccount idx
                                ]
                                ( state.allAccounts <#> \account -> HH.option [ HP.value $ display account ] [ HH.text $ display account ]
                                )
                            ]
                        , HH.th []
                            [ HH.div [ classes' "select" ]
                                [ HH.select
                                    [ HE.onSelectedIndexChange \idx ->
                                        ModifySearchQuery
                                          ( \p -> p
                                              { isExpense =
                                                  case idx of
                                                    0 -> Nothing
                                                    1 -> Just true
                                                    2 -> Just false
                                                    _ -> Nothing
                                              }
                                          )
                                    ]
                                    [ HH.option [ HP.selected $ state.searchParams.isExpense == Nothing ] [ HH.text "-" ]
                                    , HH.option [ HP.selected $ state.searchParams.isExpense == Just true ] [ HH.text "Yes" ]
                                    , HH.option [ HP.selected $ state.searchParams.isExpense == Just false ] [ HH.text "No" ]
                                    ]
                                ]
                            ]
                        -- Column for the "menu" button
                        , HH.th [] []
                        ]
                    ]
                , HH.tbody [] $
                    state.transactions
                      >>= \txGroup ->
                        case Arr.unsnoc txGroup of
                          Nothing -> []
                          Just { init, last } ->
                            (renderTransaction state false <$> init)
                              <>
                                [ renderTransaction state true last ]
                ]
            ]
        ]
    , HtmlUtils.displayWhenJust state.isSelectingTagFor \txItemId ->
        HH.slot
          _selectTag
          unit
          SelectTag.component
          { allTags: state.allTags
          }
          (HandleSelectTagOutput txItemId)
    , HtmlUtils.displayWhenJust state.splitModal \modal ->
        HH.slot
          _splitTransactionModal
          unit
          SplitTransactionModal.component
          { transactionId: modal.transactionId
          , allTags: state.allTags
          , items: modal.items
          , totalAmountCents: modal.totalAmountCents
          }
          HandleSplitModal
    ]

renderTableHeader :: forall w. State -> String -> Maybe SortingColumn -> String -> HH.HTML w Action
renderTableHeader state columnName sortingColumn styles =
  case sortingColumn of
    Nothing ->
      HH.th [ HP.style styles ]
        [ HH.text columnName ]
    Just sortingColumn ->
      HH.th [ HP.style styles ]
        [ HH.div [ classes' "columns is-clickable is-mobile", HE.onClick (\_ -> SelectedSortColumn sortingColumn) ] $
            [ HH.div [ classes' "column is-narrow" ]
                [ HH.text columnName
                ]
            , HH.div [ classes' "column is-narrow" ]
                [ displaySortIcon sortingColumn state.sorted
                ]
            ]
        ]

displaySortIcon :: forall w i. SortingColumn -> Sorted -> HH.HTML w i
displaySortIcon thisColumn sorted =
  case sorted of
    { column: c, order: o } | c == thisColumn ->
      displaySortIcon' (Just o)
    _ -> displaySortIcon' Nothing

  where
  displaySortIcon' :: Maybe SortedOrder -> HH.HTML w i
  displaySortIcon' =
    case _ of
      Nothing -> HtmlUtils.iconify "cuida:swap-vertical-arrows-outline" [ HP.style $ fontSize <> "opacity: 0.3;" ]
      Just Asc -> HtmlUtils.iconify "cuida:sort-ascending-duotone" [ HP.style fontSize ]
      Just Desc -> HtmlUtils.iconify "cuida:sort-descending-duotone" [ HP.style fontSize ]

  fontSize = "font-size: 24px;"

renderTransaction :: forall w. State -> Boolean -> TransactionItem -> HH.HTML w Action
renderTransaction state lastInGroup tx =
  HH.tr [ classes' $ if lastInGroup then "last_in_group" else "" ]
    [ HH.td
        []
        [ HH.p []
            [ HH.text $ display tx.date ]
        ]
    , HH.td
        [ HP.style "white-space: pre;" ] -- preserve contiguous space characters in the description
        [ HH.text $ tx.desc ]
    , HH.td
        [ classes' $ "has-text-right" #
            HtmlUtils.addClassIf (tx.itemAmountCents < 0) "has-background-success-light"
        ]
        [ HH.text $ Utils.centsToEuros tx.itemAmountCents ]
    , let
        addAdminAttrs attrs =
          if state.isAdmin then
            attrs <>
              [ HE.onClick \_ -> OpenSelectTagModal tx
              ]
          else attrs
        addAdminClasses cls =
          HtmlUtils.addClassIf state.isAdmin "is-clickable" cls
      in
        case tx.tag of
          Just tagName ->
            HH.td
              (addAdminAttrs [ classes' (addAdminClasses "") ])
              [ HH.text $ display tagName ]
          Nothing ->
            HH.td
              (addAdminAttrs [ classes' (addAdminClasses "has-background-danger-light") ])
              [ HH.em [ HP.style "color: #888;" ] [ HH.text "No tag" ] ]
    , HH.td
        []
        [ if state.isAdmin then
            HH.div
              [ classes' "is-clickable"
              , HH.attr (HH.AttrName "contenteditable") "plaintext-only"
              , HE.onBlur \ev -> SetTransactionDetails tx ev
              ]
              [ HH.text tx.details ]
          else
            HH.text tx.details
        ]
    , HH.td
        []
        [ HH.text $ tx.account ]
    , HH.td
        [ classes' "has-text-centered" ]
        [ HH.input $
            [ classes' "is-checkradio"
            , HP.type_ InputCheckbox
            , if tx.isExpense then HP.checked true else HP.checked false
            ]
              <>
                ( if state.isAdmin then
                    [ HE.onChecked (SetTransactionIsExpense tx)
                    , HP.disabled false
                    ]
                  else
                    [ HP.disabled true ]
                )
        ]
    , HH.td
        []
        let
          isOpen =
            case state.menuOpened of
              Nothing -> false
              Just openMenuState -> openMenuState.transactionId == tx.transactionId && openMenuState.itemIndex == tx.itemIndex
        in
          [ HH.div
              [ classes' $ "dropdown is-right"
                  # HtmlUtils.addClassIf (isOpen) "is-active"
              ]
              [ HH.div
                  [ classes' "dropdown-trigger"
                  , if isOpen then HE.onClick (\_ -> CloseMenu)
                    else HE.onClick (\ev -> OpenMenu ev { transactionId: tx.transactionId, itemIndex: tx.itemIndex })

                  ]
                  [ HH.button [ classes' "button is-small" ]
                      [ HH.span [ classes' "icon" ]
                          [ HH.i [ classes' "fas fa-solid fa-ellipsis" ] [] ]
                      ]
                  ]
              , HH.div [ classes' "dropdown-menu" ]
                  [ HH.div [ classes' "dropdown-content" ]
                      ( [ HH.a
                            [ classes' "dropdown-item", HE.onClick \_ -> CopyIdToClipboard tx.transactionId ]
                            [ HH.text "Copy ID" ]
                        ]
                          <>
                            if state.isAdmin then
                              [ HH.hr [ classes' "dropdown-divider" ]
                              , HH.a
                                  [ classes' "dropdown-item"
                                  , HE.onClick \_ -> OpenSplitTransaction tx
                                  ]
                                  [ HH.text "Split transaction" ]
                              ]
                            else
                              []
                      )
                  ]
              ]
          ]
    ]

handleAction :: forall o m. MonadAff m => MonadNavigate AppRoute m => Action -> H.HalogenM State Action Slots o m Unit
handleAction =
  case _ of
    Initialize -> do
      -- Initialize the `Choices.js` controls for the `select` boxes.
      tagChoicesControl <- H.liftEffect $ Choices.init "tag-choices"
      accountChoicesControl <- H.liftEffect $ Choices.init "account-choices"
      H.modify_ _
        { tagChoicesControl = tagChoicesControl
        , accountChoicesControl = accountChoicesControl
        }

      updateControls
      H.get >>= doSearch >>= H.put

    -- NOTE: this is not working, for some reason.
    -- We can find the element, but `focus` does not cause it to get focus...
    -- Anyway, it doesn't matter, I can just press the Tab key to focus it.
    -- Keeping the code here in case this Ref technique is helpful in the future.
    -- https://stackoverflow.com/a/64253806/857807
    -- https://www.ersocon.net/articles/focus-input-element-in-halogen-purescript%7E802632d9-d9f4-4c37-96f9-aaf920d8c599
    --
    -- H.getHTMLElementRef searchInputRef >>= case _ of
    --   Nothing -> Console.error "Search input not found"
    --   Just searchInput -> H.liftEffect $ Element.focus searchInput

    SelectedSortColumn selectedColumn -> do
      state <- H.get

      let
        newOrder =
          if selectedColumn == state.sorted.column then
            case state.sorted.order of
              Asc -> Desc
              Desc -> Asc
          else
            Sorting.defaultOrder

      let newSorted = { column: selectedColumn, order: newOrder }

      Console.log $ "[Search] Navigating to Search route: " <> show newSorted
      R.navigate $ Routes.mkSearchAppRoute (mkSearchRoute state.searchParams newSorted) false

    Receive input@{ enabled, searchRoute, isAdmin } -> do
      Console.log $ "[Search] Received new input: " <> show input
      H.modify_ _ { enabled = enabled, isAdmin = isAdmin }

      state1 <- H.get
      let oldSearchParams = state1.searchParams
      let oldSorted = state1.sorted
      let newSearchParams = searchRouteParams searchRoute
      let newSorted = searchRouteSorted searchRoute

      let state2 = state1 { searchParams = newSearchParams, sorted = newSorted }

      state3 <-
        if newSearchParams /= oldSearchParams then
          doSearch state2
        else
          pure state2

      let
        state4 =
          if newSorted /= oldSorted then do
            doSort state3
          else
            state3

      H.put state4

      updateControls

    OpenMenu ev { transactionId, itemIndex } -> do
      -- NOTE: don't let the event bubble up to `window.document`,
      -- otherwise the dropdown will close immediately.
      H.liftEffect $ E.stopPropagation $ ME.toEvent ev

      -- If the menu for another transaction was already open, close its subscriber.
      state <- H.get
      whenJust state.menuOpened \{ subscriptionId } -> do
        H.unsubscribe subscriptionId

      document <- H.liftEffect $ Window.document =<< HTML.window
      sid <- H.subscribe $
        QE.eventListener
          MET.click
          (HTMLDocument.toEventTarget document)
          (\_ -> Just CloseMenu)

      H.put state
        { menuOpened = Just
            { transactionId
            , itemIndex
            , subscriptionId: sid
            }
        }

    CloseMenu -> do
      state <- H.get
      case state.menuOpened of
        Nothing -> Console.error "[Search] Attempted to close menu when none was open"
        Just { subscriptionId } -> do
          H.unsubscribe subscriptionId
          H.put state { menuOpened = Nothing }
      pure unit

    CopyIdToClipboard txId -> do
      clipboardMb <- H.liftEffect do
        navigator <- Window.navigator =<< HTML.window
        Clip.clipboard navigator

      case clipboardMb of
        Nothing -> Console.error "[Search] Clipboard API not available"
        Just clipboard -> do
          -- NOTE: there are 2 different implementations of `Promise`.
          --   1. `purescript-aff-promise` package: https://pursuit.purescript.org/packages/purescript-aff-promise/4.0.0/docs/Control.Promise#t:Promise
          --   2. `purescript-js-promise`: https://pursuit.purescript.org/packages/purescript-js-promise-aff/1.0.0/docs/Promise.Aff#t:Promise
          --
          -- Here, we're using `toAffE` from the 2nd package because that's what the clipboard package uses.
          H.liftAff $ JsPromise.toAffE $ Clip.writeText txId clipboard

    ModifySearchQuery f -> do
      state <- H.get
      let newSearchParams = f state.searchParams

      Console.log $ "[Search] Navigating to Search route: " <> show newSearchParams
      R.navigate $ Routes.mkSearchAppRoute (mkSearchRoute newSearchParams state.sorted) false

    SelectAccount idx -> do
      allAccounts <- H.gets _.allAccounts
      let
        account =
          -- The user clicked the "x" button to remove the account selection
          if idx == -1 then
            Nothing
          else
            Arr.index allAccounts idx

      Console.log $ "[Search] Selected account: '" <> show account <> "', navigating to Search route"
      handleAction $ ModifySearchQuery _
        { account = account
        }

    SelectTag idx -> do
      allTags <- H.gets _.allTags
      let
        tagParams =
          -- The user clicked the "x" button to remove the tag selection
          if idx == -1 then
            Nothing
          else if idx == 0 then
            Just NoTag
          else
            Arr.index allTags (idx - 1) <#> SomeTag

      Console.log $ "[Search] Selected tag: '" <> show tagParams <> "', navigating to Search route"
      handleAction $ ModifySearchQuery _
        { tag = tagParams
        }

    SetTransactionIsExpense tx isExpense -> do
      state <- H.get
      updatedTx <- H.liftAff $ API.updateTransaction
        { transactionId: tx.transactionId
        , itemIndex: tx.itemIndex
        , actionType: API.ModifyIsExpense
            { isExpense
            }
        }
      H.modify_ _ { transactions = replaceTransactions state.sorted state.transactions updatedTx }

      Console.log $ "[Search] Updated isExpense for txs:"
      Console.log $ " - " <> show updatedTx

    OpenSelectTagModal txItemId -> do
      H.modify_ _ { isSelectingTagFor = Just txItemId }

    HandleSelectTagOutput tx output -> do
      -- Close the modal
      H.modify_ _ { isSelectingTagFor = Nothing }
      case output of
        SelectTag.RaiseClosedModal ->
          pure unit
        SelectTag.RaiseTagSelected tag -> do
          state <- H.get
          updatedTx <- H.liftAff $ API.updateTransaction
            { transactionId: tx.transactionId
            , itemIndex: tx.itemIndex
            , actionType: API.ModifyTag
                { tag
                }
            }
          H.modify_ _ { transactions = replaceTransactions state.sorted state.transactions updatedTx }

          Console.log $ "[Search] Updated tag for txs:"
          Console.log $ " - " <> show updatedTx

    SetTransactionDetails tx focusEv -> do
      newDetails <-
        H.liftEffect $
          focusEv
            # FocusEvent.toEvent
            # E.target
            # Utils.unsafeFromJust "Failed to get focus event's target"
            # HTMLElement.fromEventTarget
            # Utils.unsafeFromJust "Failed to get HTMLElement from event target"
            # HTMLElement.toNode
            # DOMNode.textContent
            <#> Str.trim
      state <- H.get
      updatedTx <- H.liftAff $ API.updateTransaction
        { transactionId: tx.transactionId
        , itemIndex: tx.itemIndex
        , actionType: API.ModifyDetails
            { details: newDetails
            }
        }
      H.modify_ _ { transactions = replaceTransactions state.sorted state.transactions updatedTx }

      Console.log $ "[Search] Updated details for tx:"
      Console.log $ " - " <> updatedTx.transactionId <> "#" <> show updatedTx.itemIndex

    OpenSplitTransaction tx -> do
      Console.log $ "[Search] Opening split modal for transaction " <> tx.transactionId
      items <- H.liftAff $ API.getTransactionItems tx.transactionId
      H.modify_ _
        { splitModal = Just
            { transactionId: tx.transactionId
            , items
            , totalAmountCents: tx.totalAmountCents
            }
        }

    HandleSplitModal output -> do
      case output of
        SplitTransactionModal.ModalClosed ->
          H.modify_ _ { splitModal = Nothing }
        SplitTransactionModal.SubmitSplit items -> do
          state <- H.get
          case state.splitModal of
            Nothing -> Console.error "[Search] Split modal closed without state"
            Just { transactionId } -> do
              Console.log $ "[Search] Saving split items for " <> transactionId
              _ <- H.liftAff $ API.splitTransaction transactionId items
              H.modify_ _ { splitModal = Nothing }
              refreshed <- H.get >>= doSearch
              H.put refreshed

doSearch :: forall m. MonadAff m => State -> m State
doSearch state = do
  Console.log $ "[Search] Running search"
  txs <- H.liftAff $ API.search state.searchParams

  pure $ state
    { transactions = sortTransactions txs state.sorted
    }

doSort :: State -> State
doSort state = do
  let transactions = sortTransactions (join state.transactions) state.sorted
  state { transactions = transactions }

-- Update the `Choices.js` controls to reflect the current search parameters.
updateControls :: forall o m. MonadEffect m => H.HalogenM State Action Slots o m Unit
updateControls = do
  Console.log $ "[Search] Updating controls"
  state <- H.get

  H.liftEffect $
    case state.searchParams.tag of
      Nothing -> do
        Choices.clearSelection state.tagChoicesControl
      Just NoTag ->
        Choices.selectValue state.tagChoicesControl "No tag"
      Just (SomeTag tagName) -> do
        Choices.selectValue state.tagChoicesControl (display tagName)

  H.liftEffect $
    case state.searchParams.account of
      Nothing -> do
        Choices.clearSelection state.accountChoicesControl
      Just account -> do
        Choices.selectValue state.accountChoicesControl account

replaceTransactions :: Sorted -> Array (Array TransactionItem) -> TransactionItem -> Array (Array TransactionItem)
replaceTransactions sorted groups updatedTx =
  let
    flattened = join groups
    replaced = flattened <#> \existingTx ->
      if existingTx.transactionId == updatedTx.transactionId && existingTx.itemIndex == updatedTx.itemIndex then
        updatedTx
      else
        existingTx
  in
    sortTransactions replaced sorted

searchRouteParams :: Routes.SearchRoute -> API.RawSearchParams
searchRouteParams route =
  { allFields: route.allFields
  , date: route.date
  , account: route.account
  , desc: route.desc
  , amount: route.amount
  , tag: route.tag
  , notes: route.notes
  , isExpense: route.isExpense
  }

searchRouteSorted :: Routes.SearchRoute -> Sorting.Sorted
searchRouteSorted route =
  { column: route.column
  , order: route.order
  }

mkSearchRoute :: API.RawSearchParams -> Sorting.Sorted -> Routes.SearchRoute
mkSearchRoute params sorted = Record.merge params sorted
