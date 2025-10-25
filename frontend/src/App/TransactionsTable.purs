module App.TransactionsTable where

import Prelude

import App.SelectTag as SelectTag
import App.SplitTransactionModal as SplitTransactionModal
import Core.API as API
import Core.APITypes (TagName, TransactionId, TransactionItem)
import Core.APITypes as API
import Core.Display (display)
import Core.Sorting (Sorted, SortedOrder(..), SortingColumn(..))
import Core.Sorting as Sorting
import Data.Array as Arr
import Data.Maybe (Maybe(..))
import Data.Ord.Down (Down(..))
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Promise.Aff as JsPromise
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

type Slots =
  ( selectTag :: SelectTag.Slot Unit
  , splitTransactionModal :: SplitTransactionModal.Slot Unit
  )

_selectTag = Proxy :: Proxy "selectTag"
_splitTransactionModal = Proxy :: Proxy "splitTransactionModal"

type Slot id = forall query. H.Slot query Output id

data Output = TransactionsUpdated

type Input =
  { transactions :: Array TransactionItem
  , isAdmin :: Boolean
  , allTags :: Array TagName
  }

type State =
  { transactions :: Array TransactionItem
  , sorted :: Sorted
  , allTags :: Array TagName
  -- Details about the transaction whose menu is open.
  , menuOpened :: Maybe OpenMenuState
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
  | Receive Input
  | OpenMenu ME.MouseEvent { transactionId :: TransactionId, itemIndex :: Int }
  | CloseMenu
  | CopyIdToClipboard TransactionId
  | MarkAsNonExpense TransactionItem
  | SetTransactionDetails TransactionItem FocusEvent.FocusEvent
  -- Tag Modal
  | OpenSelectTagModal TransactionItem
  | HandleSelectTagOutput TransactionItem SelectTag.Output
  -- Split Transaction Modal
  | OpenSplitTransaction TransactionItem
  | HandleSplitModal SplitTransactionModal.Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: initState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initState :: Input -> State
initState { transactions, isAdmin, allTags } =
  let
    sorted = Sorting.defaultSorted
  in
    { transactions: sortTransactions transactions sorted
    , sorted
    , menuOpened: Nothing
    , isAdmin
    , isSelectingTagFor: Nothing
    , allTags
    , splitModal: Nothing
    }

sortTransactions :: Array TransactionItem -> Sorted -> Array TransactionItem
sortTransactions transactions { column, order } =
  case column of
    Date ->
      case order of
        Asc -> Arr.sortWith (\tx -> tx.date /\ tx.transactionId /\ tx.itemIndex) transactions
        Desc -> Arr.sortWith (\tx -> Down $ tx.date /\ tx.transactionId /\ tx.itemIndex) transactions
    Tag ->
      case order of
        Asc -> Arr.sortWith (\tx -> tx.tag /\ tx.transactionId /\ tx.itemIndex) transactions
        Desc -> Arr.sortWith (\tx -> Down $ tx.tag /\ tx.transactionId /\ tx.itemIndex) transactions
    Amount ->
      case order of
        Asc -> Arr.sortWith (\tx -> tx.itemAmountCents /\ tx.transactionId /\ tx.itemIndex) transactions
        Desc -> Arr.sortWith (\tx -> Down $ tx.itemAmountCents /\ tx.transactionId /\ tx.itemIndex) transactions

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
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
                , renderTableHeader state "" Nothing "min-width: 50px;"
                ]
            ]
        , HH.tbody []
            (renderTransaction state <$> state.transactions)
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

renderTransaction :: forall w. State -> TransactionItem -> HH.HTML w Action
renderTransaction state tx =
  HH.tr []
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
                  [ HH.div [ classes' "dropdown-content" ] $
                      [ HH.a
                          [ classes' "dropdown-item", HE.onClick \_ -> CopyIdToClipboard tx.transactionId ]
                          [ HH.text "Copy ID" ]
                      ]
                        <>
                          ( if state.isAdmin then
                              [ HH.hr [ classes' "dropdown-divider" ]
                              , HH.a
                                  [ classes' "dropdown-item"
                                  , HE.onClick \_ -> OpenSplitTransaction tx
                                  ]
                                  [ HH.text "Split transaction" ]
                              , HH.hr [ classes' "dropdown-divider" ]
                              , HH.a
                                  [ classes' "dropdown-item"
                                  , HE.onClick \_ -> MarkAsNonExpense tx
                                  ]
                                  [ HH.text "Mark as non-expense" ]
                              ]
                            else
                              []
                          )
                  ]
              ]
          ]
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction =
  case _ of
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

      let transactions = sortTransactions state.transactions newSorted

      H.put state { transactions = transactions, sorted = newSorted }
    Receive input -> do
      sorted <- H.gets _.sorted
      let transactions = sortTransactions input.transactions sorted
      H.modify_ \s -> s { transactions = transactions }

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
        Nothing -> Console.error "Attempted to close menu when none was open"
        Just { subscriptionId } -> do
          H.unsubscribe subscriptionId
          H.put state { menuOpened = Nothing }
      pure unit

    CopyIdToClipboard txId -> do
      clipboardMb <- H.liftEffect do
        navigator <- Window.navigator =<< HTML.window
        Clip.clipboard navigator

      case clipboardMb of
        Nothing -> Console.error "Clipboard API not available"
        Just clipboard -> do
          -- NOTE: there are 2 different implementations of `Promise`.
          --   1. `purescript-aff-promise` package: https://pursuit.purescript.org/packages/purescript-aff-promise/4.0.0/docs/Control.Promise#t:Promise
          --   2. `purescript-js-promise`: https://pursuit.purescript.org/packages/purescript-js-promise-aff/1.0.0/docs/Promise.Aff#t:Promise
          --
          -- Here, we're using `toAffE` from the 2nd package because that's what the clipboard package uses.
          H.liftAff $ JsPromise.toAffE $ Clip.writeText txId clipboard

    MarkAsNonExpense tx -> do
      _ <- H.liftAff $ API.updateTransaction
        { transactionId: tx.transactionId
        , itemIndex: tx.itemIndex
        , actionType: API.ModifyIsExpense
            { isExpense: false
            }
        }
      H.raise $ TransactionsUpdated
      Console.log $ "[Transactions] Marked transaction as non-expense: " <> tx.transactionId

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
      _ <- H.liftAff $ API.updateTransaction
        { transactionId: tx.transactionId
        , itemIndex: tx.itemIndex
        , actionType: API.ModifyDetails
            { details: newDetails
            }
        }
      H.raise $ TransactionsUpdated
      Console.log $ "[Transactions] Updated details for transaction: " <> tx.transactionId

    OpenSelectTagModal txItemId -> do
      H.modify_ _ { isSelectingTagFor = Just txItemId }

    HandleSelectTagOutput tx output -> do
      -- Close the modal
      H.modify_ _ { isSelectingTagFor = Nothing }
      case output of
        SelectTag.RaiseClosedModal ->
          pure unit
        SelectTag.RaiseTagSelected tag -> do
          _ <- H.liftAff $ API.updateTransaction
            { transactionId: tx.transactionId
            , itemIndex: tx.itemIndex
            , actionType: API.ModifyTag
                { tag
                }
            }
          H.raise $ TransactionsUpdated
          Console.log $ "[Transactions] Updated tag for transaction: " <> tx.transactionId

    OpenSplitTransaction tx -> do
      Console.log $ "[Transactions] Opening split modal for transaction " <> tx.transactionId
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
            Nothing -> Console.error "[Transactions] Split modal closed without state"
            Just { transactionId } -> do
              Console.log $ "[Transactions] Saving split items for " <> transactionId
              _ <- H.liftAff $ API.splitTransaction transactionId items
              H.modify_ _ { splitModal = Nothing }
              H.raise $ TransactionsUpdated
