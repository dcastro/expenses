module App.SplitTransactionModal where

import Prelude

import App.NewTransactionModal.AmountInput as AmountInput
import App.SelectTag as SelectTag
import Core.APITypes (TagName, TransactionId)
import Core.APITypes as API
import Core.Display (display)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as Str
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (Proxy(..))
import Utils as Utils
import Web.Event.Event as E
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type Slots =
  ( selectTag :: SelectTag.Slot Unit
  )

type Slot id = forall query. H.Slot query Output id

_selectTag = Proxy :: Proxy "selectTag"

data Output
  = ModalClosed
  | SubmitSplit (Array API.NewShortTransactionItem)

type Input =
  { transactionId :: TransactionId
  , allTags :: Array TagName
  , items :: Array API.ShortTransactionItem
  , totalAmountCents :: Int
  }

type State =
  { transactionId :: TransactionId
  , allTags :: Array TagName
  , items :: Array ItemState
  , totalAmountCents :: Int
  , isSubmitting :: Boolean
  , isSelectingTag :: Maybe Int
  , keySubscription :: Maybe H.SubscriptionId
  , totalAmountError :: Maybe String
  }

type ItemState =
  { amount :: String
  , tag :: Maybe TagName
  , details :: String
  , isExpense :: Boolean
  }

type ItemForm =
  { amount :: String
  , amountError :: Maybe String
  , tag :: Maybe TagName
  , tagError :: Maybe String
  , details :: String
  }

data Action
  = Initialize
  | SubmitForm (Array API.NewShortTransactionItem) E.Event
  | CloseModal
  | UpdateAmount Int String
  | UpdateDetails Int String
  | OpenSelectTag Int
  | HandleSelectTagOutput Int SelectTag.Output
  | AddItem
  | RemoveItem Int
  | HandleKeyDown KE.KeyboardEvent

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: initState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initState :: Input -> State
initState input =
  { transactionId: input.transactionId
  , allTags: input.allTags
  , items: mkItemState input.items
  , totalAmountCents: input.totalAmountCents
  , isSubmitting: false
  , isSelectingTag: Nothing
  , keySubscription: Nothing
  , totalAmountError: Nothing
  }

mkItemState :: Array API.ShortTransactionItem -> Array ItemState
mkItemState items =
  let
    itemStates = items <#> \item ->
      { amount: Utils.centsToEurosRaw item.itemAmountCents
      , tag: item.tag
      , details: item.details
      , isExpense: item.isExpense
      }

  in
    if Array.null itemStates then [ defaultItemState ] else itemStates

defaultItemState :: ItemState
defaultItemState =
  { amount: ""
  , tag: Nothing
  , details: ""
  , isExpense: true
  }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do

    document <- H.liftEffect $ Window.document =<< HTML.window
    subscription <- H.subscribe $
      QE.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map HandleKeyDown <<< KE.fromEvent)

    H.modify_ _ { keySubscription = Just subscription }

  SubmitForm parsedItems ev -> do
    H.liftEffect $ E.preventDefault ev
    H.modify_ _ { isSubmitting = true }
    raise (SubmitSplit parsedItems)

  CloseModal ->
    raise ModalClosed

  UpdateAmount idx value -> do
    let sanitized = AmountInput.sanitizeAmountInput value
    H.modify_ \state -> state
      { items = updateItem idx state.items \item ->
          item { amount = sanitized }
      }

  UpdateDetails idx value ->
    H.modify_ \state ->
      state
        { items = updateItem idx state.items \item ->
            item { details = value }
        }

  OpenSelectTag idx ->
    H.modify_ _ { isSelectingTag = Just idx }

  HandleSelectTagOutput idx output ->
    case output of
      SelectTag.RaiseClosedModal ->
        H.modify_ _ { isSelectingTag = Nothing }
      SelectTag.RaiseTagSelected tag ->
        H.modify_ \state -> state
          { items = updateItem idx state.items \item ->
              item { tag = Just tag }
          , isSelectingTag = Nothing
          }

  AddItem ->
    H.modify_ \state ->
      state
        { items = state.items <> [ defaultItemState ] }

  RemoveItem idx ->
    H.modify_ \state ->
      if Array.length state.items <= 1 then state
      else
        state { items = fromMaybe state.items (Array.deleteAt idx state.items) }

  HandleKeyDown ev ->
    case KE.key ev of
      "Escape" -> do
        state <- H.get
        -- If the `SelectTag` modal is open, then we let it handle the `Escape` key itself.
        -- We don't want to close both modals at once.
        if isJust state.isSelectingTag then
          pure unit
        else
          handleAction CloseModal
      _ -> pure unit

type ParseResult =
  { itemForms :: Array ItemForm
  , totalAmountError :: Maybe String
  , parsedItems :: Maybe (Array API.NewShortTransactionItem)
  }

tryParseItems
  :: State
  -> ParseResult
tryParseItems state = do
  let itemForms /\ parsedItemsMb = tryParseItems' state.items

  let
    totalAmountErrorMb /\ parsedItemsMb' =
      case parsedItemsMb of
        Just parsedItems -> do
          let sumAmount = getSumAmount parsedItems
          if sumAmount == state.totalAmountCents then
            Nothing /\ Just parsedItems
          else
            Just (totalAmountWarning sumAmount) /\ Nothing
        _ -> Nothing /\ Nothing

  { itemForms
  , totalAmountError: totalAmountErrorMb
  , parsedItems: parsedItemsMb'
  }
  where
  getSumAmount :: Array API.NewShortTransactionItem -> Int
  getSumAmount items = items <#> _.itemAmountCents # sum

  tryParseItems' :: Array ItemState -> Array ItemForm /\ Maybe (Array API.NewShortTransactionItem)
  tryParseItems' items = do
    let
      (itemForms :: Array ItemForm) /\ (parsedItemsMb :: Array (Maybe API.NewShortTransactionItem)) =
        items <#> tryParseItem # Array.unzip

    itemForms /\ sequence parsedItemsMb

  totalAmountWarning :: Int -> String
  totalAmountWarning sumAmount =
    "Split amounts total is "
      <> Utils.centsToEuros sumAmount
      <> " but the transaction total is "
      <> Utils.centsToEuros state.totalAmountCents
      <> "."

tryParseItem
  :: ItemState
  -> ItemForm /\ Maybe API.NewShortTransactionItem
tryParseItem itemState = do
  let
    item1 =
      { amount: itemState.amount
      , tag: itemState.tag
      , details: itemState.details
      , amountError: Nothing
      , tagError: Nothing
      }

  let
    (item2 /\ amountMb) =
      case AmountInput.tryParseAmountCents item1.amount of
        Right amount -> item1 { amountError = Nothing } /\ Just amount
        Left err -> item1 { amountError = Just err } /\ Nothing

  let
    (item3 /\ tagMb) =
      case item1.tag of
        Just tag -> item2 { tagError = Nothing } /\ Just tag
        Nothing -> item2 { tagError = Just "Please choose a tag." } /\ Nothing

  let
    parsedItemMb = do
      amount <- amountMb
      tag <- tagMb
      pure $ { itemAmountCents: amount, tag, details: Str.trim item1.details, isExpense: itemState.isExpense }

  item3 /\ parsedItemMb

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  let
    { itemForms, totalAmountError, parsedItems } = tryParseItems state
  in
    HH.div
      [ classes' "modal is-active" ]
      [ HH.div
          [ classes' "modal-background"
          , HE.onClick (const CloseModal)
          ]
          []
      , HH.div [ classes' "modal-content" ]
          [ HH.div [ classes' "box" ]
              [ HH.h3 [ classes' "title is-4" ] [ HH.text "Split transaction" ]
              , HH.form
                  ( case parsedItems of
                      Just parsedItems -> [ HE.onSubmit $ SubmitForm parsedItems ]
                      Nothing -> []
                  )
                  [ HH.div [] (itemForms # Array.mapWithIndex renderItem)
                  , HH.div [ classes' "buttons" ]
                      [ HH.button
                          [ classes' "button is-link is-light"
                          , HE.onClick (const AddItem)
                          , HP.type_ HP.ButtonButton
                          ]
                          [ HH.span [ classes' "icon" ]
                              [ HH.i [ classes' "fas fa-plus" ] [] ]
                          , HH.span_ [ HH.text "Add item" ]
                          ]
                      ]
                  , HH.div
                      [ classes' "columns is-mobile" ]

                      [ HH.div
                          [ classes' "column" ]
                          [ HtmlUtils.displayWhenJust totalAmountError \msg -> do
                              HH.p
                                [ classes' "help is-danger" ]
                                [ HH.text msg ]
                          ]

                      , HH.div
                          [ classes' "column is-narrow field is-grouped" ]
                          [ HH.div
                              [ classes' "control"
                              ]
                              [ HH.button
                                  [ classes' $ "button is-primary"
                                      # HtmlUtils.addClassIf state.isSubmitting "is-loading"
                                  , HP.type_ HP.ButtonSubmit
                                  , HP.enabled $ canSubmit state parsedItems
                                  ]
                                  [ HH.span [ classes' "icon" ]
                                      [ HH.i [ classes' "fas fa-save" ] [] ]
                                  , HH.span_ [ HH.text "Save" ]
                                  ]
                              ]
                          , HH.p [ classes' "control" ]
                              [ HH.button
                                  [ classes' "button"
                                  , HE.onClick (const CloseModal)
                                  , HP.type_ HP.ButtonButton
                                  ]
                                  [ HH.text "Cancel" ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      , HtmlUtils.displayWhenJust state.isSelectingTag \idx ->
          HH.slot
            _selectTag
            unit
            SelectTag.component
            { allTags: state.allTags }
            (HandleSelectTagOutput idx)
      ]
  where

  renderItem idx item =
    HH.div [ classes' "box mb-4" ]
      [ HH.div [ classes' "is-flex is-align-items-center is-justify-content-space-between" ]
          [ HH.h4 [ classes' "title is-5 mb-2" ]
              [ HH.text $ "Part " <> show (idx + 1) ]
          , HH.button
              [ classes' "button is-small is-danger is-light"
              , HE.onClick (const (RemoveItem idx))
              , HP.type_ HP.ButtonButton
              , HP.disabled (Array.length state.items <= 1)
              ]
              [ HH.span [ classes' "icon" ]
                  [ HH.i [ classes' "fas fa-trash" ] [] ]
              ]
          ]
      , renderTagField idx item
      , renderAmountField idx item
      , renderDetailsField idx item
      ]

  renderAmountField idx item =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ]
          [ HH.text "Amount"
          , HH.div [ classes' "field has-addons" ]
              [ HH.div [ classes' "control is-expanded" ]
                  [ HH.input
                      [ classes' "input"
                      , HP.type_ HP.InputText
                      , HP.value item.amount
                      , HE.onValueInput (UpdateAmount idx)
                      , HH.attr (HH.AttrName "inputmode") "decimal"
                      , HP.placeholder "0.00"
                      ]
                  ]
              , HH.p [ classes' "control" ]
                  [ HH.a [ classes' "button is-static" ] [ HH.text "€" ]
                  ]
              ]
          ]
      , HH.p [ classes' "help is-danger" ]
          [ case item.amountError of
              Nothing -> HH.text "​"
              Just err -> HH.text err
          ]
      ]

  renderTagField idx item =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ] [ HH.text "Tag" ]
      , HH.div [ classes' "field is-grouped" ]
          ( [ HH.p [ classes' "control is-expanded" ]
                [ HH.input
                    [ classes' "input"
                    , HP.readOnly true
                    , HP.value $ case item.tag of
                        Nothing -> ""
                        Just selectedTag -> display selectedTag
                    , HP.placeholder "No tag selected"
                    ]
                ]
            , HH.p [ classes' "control" ]
                [ HH.button
                    [ classes' "button"
                    , HE.onClick (const (OpenSelectTag idx))
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.span [ classes' "icon" ]
                        [ HH.i [ classes' "fas fa-tag" ] [] ]
                    , HH.span_ [ HH.text "Choose" ]
                    ]
                ]
            ]
          )
      , HH.p [ classes' "help is-danger" ]
          [ case item.tagError of
              Nothing -> HH.text ""
              Just err -> HH.text err
          ]
      ]

  renderDetailsField idx item =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ]
          [ HH.text "Details"
          , HH.div [ classes' "control" ]
              [ HH.textarea
                  [ classes' "textarea"
                  , HP.rows 2
                  , HP.placeholder "Description"
                  , HE.onValueInput (UpdateDetails idx)
                  , HP.value item.details
                  , HP.spellcheck true
                  ]
              ]
          ]
      ]

  canSubmit :: State -> Maybe (Array API.NewShortTransactionItem) -> Boolean
  canSubmit st parsedItemsMb =
    not st.isSubmitting
      && isJust parsedItemsMb

updateItem :: forall a. Int -> Array a -> (a -> a) -> Array a
updateItem idx items f =
  case Array.modifyAt idx f items of
    Just updatedItems -> updatedItems
    Nothing -> unsafeCrashWith $
      "updateItem: index out of bounds: invalid index " <> show idx <> " for array of length " <> show (Array.length items)

-- Use this function to ensure event subscriptions are cleaned up before quitting the modal
raise :: forall m. Output -> H.HalogenM State Action Slots Output m Unit
raise output = do
  state <- H.get
  Utils.whenJust state.keySubscription \subId -> do
    H.unsubscribe subId

  H.raise output
