module App.NewTransactionModal where

import Prelude

import App.NewTransactionModal.AmountInput as AmountInput
import App.SelectTag as SelectTag
import Control.Monad.Except (throwError)
import Core.API as API
import Core.APITypes (NewTransactionItem(..), TagName)
import Core.APITypes as API
import Core.Display (display)
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Either as Either
import Data.Formatter.DateTime as F
import Data.Maybe (Maybe(..), isJust)
import Data.String as Str
import Effect.Aff.Class (class MonadAff)
import Effect.Now as Now
import Effect.Unsafe as Unsafe
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
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
  | TransactionCreated

type Input =
  { allTags :: Array TagName
  , allAccounts :: Array String
  }

type State =
  { allTags :: Array TagName
  , allAccounts :: Array String

  , isSubmitting :: Boolean
  , isSelectingTag :: Boolean
  , keySubscription :: Maybe H.SubscriptionId

  -- Form fields
  , date :: String
  , amountCents :: String
  , selectedAccount :: String
  , selectedTag :: Maybe TagName
  , details :: String
  }

data Action
  = UpdateDate String
  | UpdateAmount String
  | UpdateAccount Int
  | UpdateDetails String
  | SubmitForm NewTransactionItem E.Event
  | CloseModal
  | OpenSelectTagModal
  | HandleSelectTagOutput SelectTag.Output
  | Initialize
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
initState { allTags, allAccounts } =
  let
    defaultAccount = Utils.unsafeFromJust "NewTransactionModal: expected at least one account" $ Array.head allAccounts
  in
    { allTags
    , allAccounts
    , date: ""
    , amountCents: ""
    , selectedAccount: defaultAccount
    , selectedTag: Nothing
    , details: ""
    , isSubmitting: false
    , isSelectingTag: false
    , keySubscription: Nothing
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = do
  let parseResult = tryParseNewTransaction state

  HH.div
    [ classes' "modal is-active" ]
    [ HH.div
        [ classes' "modal-background"
        , HE.onClick (const CloseModal)
        ]
        []
    , HH.div [ classes' "modal-content" ]
        [ HH.div [ classes' "box" ]
            [ HH.h3 [ classes' "title is-4" ] [ HH.text "New transaction" ]
            , HH.form
                ( case parseResult of
                    Left _ -> []
                    Right newTx -> [ HE.onSubmit (SubmitForm newTx) ]
                )
                [ renderDateField state.date
                , renderAccountField state.selectedAccount state.allAccounts
                , renderTagField state.selectedTag
                , renderAmountField state.amountCents parseResult
                , renderDetailsField state.details
                , HH.div [ classes' "field is-grouped is-grouped-right" ]
                    [ HH.p [ classes' "control" ]
                        [ HH.button
                            [ classes' $
                                "button is-primary"
                                  # HtmlUtils.addClassIf state.isSubmitting "is-loading"
                            , HP.type_ HP.ButtonSubmit
                            , HP.enabled $ canSubmit state (Either.hush parseResult)
                            ]
                            [ HH.span [ classes' "icon" ]
                                [ HH.i [ classes' "fas fa-plus" ] [] ]
                            , HH.span_ [ HH.text "Create" ]
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
    , HtmlUtils.displayIf state.isSelectingTag $
        HH.slot
          _selectTag
          unit
          SelectTag.component
          { allTags: state.allTags }
          HandleSelectTagOutput
    ]
  where
  renderDateField value =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ]
          [ HH.text "Date"
          , HH.div [ classes' "control" ]
              [ HH.input
                  [ classes' "input"
                  , HP.type_ HP.InputDate
                  , HP.value value
                  , HE.onValueInput UpdateDate
                  , HP.required true
                  ]
              ]
          ]
      ]

  renderAccountField selected allAccounts =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ]
          [ HH.text "Account"
          , HH.div [ classes' "control" ]
              [ HH.div [ classes' "select is-fullwidth" ]
                  [ HH.select
                      [ HE.onSelectedIndexChange UpdateAccount
                      , HP.value selected
                      , HP.required true
                      ]
                      ( allAccounts <#> \account ->
                          HH.option
                            [ HP.value account
                            , HP.selected (account == selected)
                            ]
                            [ HH.text account ]
                      )
                  ]
              ]
          ]
      ]

  renderAmountField value (parseResult :: Either ParseErrors _) =
    -- NOTE: using a nested `.field` to make `has-addons` work with `.help`
    -- See: https://github.com/jgthms/bulma/issues/640#issuecomment-415231212
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ]
          [ HH.text "Amount"
          , HH.div [ classes' "field has-addons" ]
              [ HH.div [ classes' "control is-expanded" ]
                  [ HH.input
                      [ classes' "input"
                      , HP.type_ HP.InputText
                      , HP.value value
                      {-
                        NOTE: correcting the input value on every change is problematic,
                        see this discussion: https://discourse.purescript.org/t/halogen-how-to-rerender/2466/6
                        Halogen doesn't always update the input's value in the DOM.
                        A user suggested hooking into `onKeyDown` to ignore characters that are not allowed,
                        but that didn't seem to work on Chrome on Android.
                        So I'm just letting the user type anything and then present them with an error message if the input is invalid.
                      -}
                      , HP.placeholder "0.00"
                      , HE.onValueInput UpdateAmount
                      , HH.attr (HH.AttrName "inputmode") "decimal"
                      ]
                  ]
              , HH.p [ classes' "control" ]
                  [ HH.a [ classes' "button is-static" ] [ HH.text "€" ]
                  ]
              ]
          ]
      , HH.p [ classes' "help is-danger" ]
          [ case parseResult of
              Left { amountError: Just err } ->
                HH.text err
              _ ->
                -- NOTE: use a zero-width space U+200B to keep the help text height consistent.
                -- https://unicode-explorer.com/c/200B
                HH.text "​"
          ]
      ]

  renderTagField selected =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ] [ HH.text "Tag" ]
      , HH.div [ classes' "field is-grouped" ]
          ( [ HH.p [ classes' "control is-expanded" ]
                [ HH.input
                    [ classes' "input"
                    , HP.readOnly true
                    , HP.value $ case selected of
                        Nothing -> ""
                        Just tagName -> display tagName
                    , HP.placeholder "No tag selected"
                    ]
                ]
            , HH.p [ classes' "control" ]
                [ HH.button
                    [ classes' "button"
                    , HE.onClick (const OpenSelectTagModal)
                    -- NOTE: without `type="button"`, the button would act as a submit button.
                    -- https://stackoverflow.com/a/2825867
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.span [ classes' "icon" ]
                        [ HH.i [ classes' "fas fa-tag" ] [] ]
                    , HH.span_ [ HH.text "Choose" ]
                    ]
                ]
            ]
          )
      ]

  renderDetailsField value =
    HH.div [ classes' "field" ]
      [ HH.label [ classes' "label" ]
          [ HH.text "Notes"
          , HH.div [ classes' "control" ]
              [ HH.textarea
                  [ classes' "textarea"
                  , HE.onValueInput UpdateDetails
                  , HP.value value
                  , HP.rows 3
                  , HP.placeholder "Description or notes"
                  , HP.spellcheck true
                  ]
              ]
          ]
      ]

canSubmit :: State -> Maybe NewTransactionItem -> Boolean
canSubmit state newTxMb =
  not state.isSubmitting
    && isJust newTxMb

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    today <- H.liftEffect Now.nowDate
      <#> (\date -> DateTime date bottom)
      <#> F.formatDateTime "YYYY-MM-DD"
      <#> Utils.unsafeFromRight

    document <- H.liftEffect $ Window.document =<< HTML.window
    subscription <- H.subscribe $
      QE.eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map HandleKeyDown <<< KE.fromEvent)

    H.modify_ \state -> state
      { keySubscription = Just subscription
      , date = if state.date == "" then today else state.date
      }

  UpdateDate value ->
    H.modify_ _ { date = value }

  UpdateAmount value -> do
    H.modify_ _ { amountCents = AmountInput.sanitizeAmountInput value }

  UpdateAccount idx -> do
    allAccounts <- H.gets _.allAccounts
    let
      selectedAccount = Array.index allAccounts idx
        # Utils.unsafeFromJust "NewTransactionModal: invalid account index"

    H.modify_ _ { selectedAccount = selectedAccount }

  UpdateDetails value ->
    H.modify_ _ { details = value }

  OpenSelectTagModal -> do
    H.modify_ _ { isSelectingTag = true }

  HandleSelectTagOutput output -> do
    case output of
      SelectTag.RaiseClosedModal -> do
        H.modify_ _ { isSelectingTag = false }
      SelectTag.RaiseTagSelected tag -> do
        H.modify_ _
          { selectedTag = Just tag
          , isSelectingTag = false
          }

  HandleKeyDown ev ->
    case KE.key ev of
      "Escape" -> do
        state <- H.get
        -- If the `SelectTag` modal is open, then we let it handle the `Escape` key itself.
        -- We don't want to close both modals at once.
        if state.isSelectingTag then
          pure unit
        else
          handleAction CloseModal
      _ -> pure unit

  SubmitForm newTx ev -> do
    H.liftEffect $ E.preventDefault ev

    H.modify_ _ { isSubmitting = true }
    _ <- H.liftAff $ API.createTransaction newTx
    H.modify_ _ { isSubmitting = false }
    raise TransactionCreated

  CloseModal -> do
    H.modify_ _ { isSelectingTag = false }
    raise ModalClosed

type ParseErrors =
  { amountError :: Maybe String
  }

tryParseNewTransaction :: State -> Either ParseErrors NewTransactionItem
tryParseNewTransaction state = do

  dateStr <- requireField state.date
  jdate <- case Unsafe.unsafePerformEffect (API.parseJDate dateStr) of
    Nothing -> throwError noError
    Just value -> pure value

  amountStr <- requireField state.amountCents
  totalAmountCents <- case AmountInput.tryParseAmountCents amountStr of
    Left err -> throwError { amountError: Just err }
    Right cents -> pure cents

  tag <- case state.selectedTag of
    Nothing -> throwError noError
    Just value -> pure value

  let
    details = Str.trim state.details

  pure $ NewTransactionItem
    { date: jdate
    , totalAmountCents
    , tag
    , details
    , account: state.selectedAccount

    -- Defaults
    , isExpense: true
    , desc: ""
    }
  where
  noError :: ParseErrors
  noError = { amountError: Nothing }

  requireField :: String -> Either ParseErrors String
  requireField value =
    let
      trimmed = Str.trim value
    in
      if trimmed == "" then throwError noError else pure trimmed

-- Use this function to ensure event subscriptions are cleaned up before quitting the modal
raise :: forall m. Output -> H.HalogenM State Action Slots Output m Unit
raise output = do
  state <- H.get
  Utils.whenJust state.keySubscription \subId -> do
    H.unsubscribe subId

  H.raise output
