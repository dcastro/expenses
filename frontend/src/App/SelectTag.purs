module App.SelectTag where

import Prelude

import Core.APITypes (TagName, getTagName)
import Core.Display (display)
import Data.Array as Arr
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as QE
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Utils as Utils
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as Element
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type Slots :: forall k. Row k
type Slots =
  (
  )

type Slot id = forall query. H.Slot query Output id

data Output
  = RaiseTagSelected TagName
  | RaiseClosedModal

type Input =
  { allTags :: Array TagName
  }

type State =
  { allTags :: Array TagName
  , searchInputText :: String
  , windowClickSubscription :: Maybe (H.SubscriptionId)
  , selected :: Maybe TagName
  }

data Action
  = Initialize
  | UpdateSearchInput String
  | SelectTag TagName
  | HandleKeyDown KE.KeyboardEvent
  | CloseModal
  | Receive Input

searchInputRef :: H.RefLabel
searchInputRef = H.RefLabel "search-input-ref"

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: initState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initState :: Input -> State
initState { allTags } =
  { allTags
  , searchInputText: ""
  , windowClickSubscription: Nothing
  , selected: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  let
    tags = filteredTags state
  in
    HH.div
      [ classes' "modal is-active"
      ]
      [ HH.div
          [ classes' "modal-background"
          , HE.onClick (const CloseModal)
          ]
          []
      , HH.div [ classes' "modal-content box" ]
          [ HH.nav [ classes' "panel" ]
              ( [ HH.p [ classes' "panel-heading is-flex is-justify-content-space-between" ]
                    [ HH.span_ [ HH.text "Select a tag" ]
                    , HH.button
                        [ classes' "delete"
                        , HE.onClick (const CloseModal)
                        ]
                        []
                    ]
                , renderSearchBlock state.searchInputText
                ]
                  <> (tags <#> renderTagBlock)
              )
          ]
      ]
  where
  renderSearchBlock searchText =
    HH.div [ classes' "panel-block" ]
      [ HH.p [ classes' "control has-icons-left" ]
          [ HH.input
              [ classes' "input"
              , HP.type_ InputSearch
              , HP.placeholder "Search tags"
              , HP.value searchText
              , HE.onValueInput UpdateSearchInput
              , HP.ref searchInputRef
              ]
          , HH.span [ classes' "icon is-left is-small" ]
              [ HH.i [ classes' "fas fa-search" ] [] ]
          ]
      ]

  renderTagBlock tagName =
    HH.a
      [ classes' $ "panel-block" # HtmlUtils.addClassIf (state.selected == Just tagName) "is-active"
      , HE.onClick (const (SelectTag tagName))
      ]
      [ HH.span [ classes' "panel-icon" ]
          [ HH.i [ classes' "fas fa-tag" ] [] ]
      , HH.text $ display tagName
      ]

filteredTags :: State -> Array TagName
filteredTags state =
  let
    query = String.toLower state.searchInputText
    matches tagName =
      let
        tagLabel = String.toLower (getTagName tagName)
      in
        query == "" || String.contains (Pattern query) tagLabel
  in
    Arr.filter matches state.allTags

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction =
  case _ of
    Initialize -> do
      document <- H.liftEffect $ Window.document =<< HTML.window
      subId <- H.subscribe $
        QE.eventListener
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (HandleKeyDown) <<< KE.fromEvent)

      -- Set focus on the search input when the modal opens.
      --
      -- https://stackoverflow.com/a/64253806/857807
      -- https://www.ersocon.net/articles/focus-input-element-in-halogen-purescript%7E802632d9-d9f4-4c37-96f9-aaf920d8c599
      H.getHTMLElementRef searchInputRef >>= case _ of
        Nothing -> Console.error "Search input not found"
        Just searchInput -> H.liftEffect $ Element.focus searchInput

      H.modify_ _ { windowClickSubscription = Just subId }

    UpdateSearchInput value ->
      H.modify_ _
        { searchInputText = value
        , selected = Nothing
        }

    SelectTag tagName ->
      raise $ RaiseTagSelected tagName

    HandleKeyDown ev -> do
      case KE.key ev of
        "Escape" -> do
          handleAction CloseModal
        "ArrowDown" -> do
          state <- H.get
          let tags = filteredTags state
          case tags of
            [] -> pure unit
            _ ->
              let
                nextSelection =
                  case state.selected of
                    Nothing -> Arr.index tags 0
                    Just current -> do
                      idx <- Arr.findIndex (_ == current) tags
                      let
                        nextIdx =
                          if idx == Arr.length tags - 1 then 0 else idx + 1
                      Arr.index tags nextIdx
              in
                case nextSelection of
                  Nothing -> pure unit
                  Just tag -> H.modify_ _ { selected = Just tag }
        "ArrowUp" -> do
          state <- H.get
          let tags = filteredTags state
          case tags of
            [] -> pure unit
            _ ->
              let
                nextSelection =
                  case state.selected of
                    Nothing -> Arr.index tags (Arr.length tags - 1)
                    Just current -> do
                      idx <- Arr.findIndex (_ == current) tags
                      let
                        prevIdx =
                          if idx == 0 then Arr.length tags - 1 else idx - 1
                      Arr.index tags prevIdx
              in
                case nextSelection of
                  Nothing -> pure unit
                  Just tag -> H.modify_ _ { selected = Just tag }
        "Enter" -> do
          state <- H.get
          case state.selected of
            Nothing -> pure unit
            Just tag -> handleAction (SelectTag tag)
        _ ->
          pure unit

    CloseModal -> do
      raise RaiseClosedModal

    Receive input ->
      H.modify_ _ { allTags = input.allTags }

-- Use this function to ensure event subscriptions are cleaned up before quitting the modal
raise :: forall m. Output -> H.HalogenM State Action Slots Output m Unit
raise output = do
  state <- H.get
  Utils.whenJust state.windowClickSubscription \subId -> do
    H.unsubscribe subId

  H.raise output
