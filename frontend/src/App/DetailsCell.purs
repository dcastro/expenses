-- | The content of the "Notes" cell of a transaction item.
-- |
-- | * If the item has details, it displays them.
-- | * If the item has no details but has a suggestion, it displays the suggestion in italic.
-- |   An admin can then either:
-- |     * approve the suggestion: it's saved as the item's details, or
-- |     * click the suggestion: it's copied to the editable area, which is focused, so the admin can edit it.
-- |       The text is saved when the area loses focus.
-- | * Otherwise, it displays an empty area.
module App.DetailsCell where

import Prelude

import Core.API as API
import Core.APITypes (TransactionItem)
import Core.APITypes as API
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Utils (whenJust)
import Web.DOM.Node as DOMNode
import Web.HTML.HTMLElement as HTMLElement

type Slot id = forall query. H.Slot query Output id

-- | Raised after the item's details are saved, with the updated item.
data Output = DetailsUpdated TransactionItem

type Input =
  { tx :: TransactionItem
  , isAdmin :: Boolean
  }

type State =
  { tx :: TransactionItem
  , isAdmin :: Boolean
  -- | Whether the admin clicked the suggestion, and is now editing it.
  , isEditingSuggestion :: Boolean
  }

data Action
  = Receive Input
  | EditSuggestion
  | ApproveSuggestion String
  | SaveDetails

editableRef :: H.RefLabel
editableRef = H.RefLabel "details-editable"

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \{ tx, isAdmin } -> { tx, isAdmin, isEditingSuggestion: false }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

-- | The item's suggestion, if it doesn't have details yet.
pendingSuggestion :: TransactionItem -> Maybe String
pendingSuggestion tx =
  if tx.details == "" then
    tx.detailsSuggestion
  else
    Nothing

-- Display line breaks, e.g. a suggestion with one item per line.
preLine :: forall r i. HP.IProp (style :: String | r) i
preLine = HP.style "white-space: pre-line;"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  case pendingSuggestion state.tx, state.isEditingSuggestion of
    Just suggestion, false ->
      if state.isAdmin then
        HH.div []
          [ HH.div
              [ classes' "is-italic has-text-grey is-clickable"
              , preLine
              , HP.title "Suggestion. Click to edit it."
              , HE.onClick \_ -> EditSuggestion
              ]
              [ HH.text suggestion ]
          , HH.button
              [ classes' "button is-small is-success is-light mt-2"
              , HE.onClick \_ -> ApproveSuggestion suggestion
              ]
              [ HH.text "Approve" ]
          ]
      else
        HH.div
          [ classes' "is-italic has-text-grey", preLine, HP.title "Suggestion" ]
          [ HH.text suggestion ]
    _, _ ->
      let
        text =
          if state.isEditingSuggestion then
            fromMaybe "" (pendingSuggestion state.tx)
          else
            state.tx.details
      in
        if state.isAdmin then
          HH.div
            [ classes' "is-clickable"
            , preLine
            , HH.attr (HH.AttrName "contenteditable") "plaintext-only"
            , HP.prop (HH.PropName "textContent") text
            , HP.ref editableRef
            , HE.onBlur \_ -> SaveDetails
            ]
            []
        else
          HH.div [ preLine ] [ HH.text text ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive { tx, isAdmin } ->
    H.modify_ _ { tx = tx, isAdmin = isAdmin }

  EditSuggestion -> do
    H.modify_ _ { isEditingSuggestion = true }
    -- The editable area has just been rendered with the suggestion's text.
    editable <- H.getHTMLElementRef editableRef
    whenJust editable \el -> H.liftEffect do
      HTMLElement.focus el
      HtmlUtils.moveCursorToEnd el

  ApproveSuggestion suggestion ->
    saveDetails suggestion

  SaveDetails -> do
    editable <- H.getHTMLElementRef editableRef
    whenJust editable \el -> do
      newDetails <- H.liftEffect $ HTMLElement.toNode el # DOMNode.textContent <#> Str.trim
      saveDetails newDetails

saveDetails :: forall m. MonadAff m => String -> H.HalogenM State Action () Output m Unit
saveDetails newDetails = do
  tx <- H.gets _.tx
  updatedTx <- H.liftAff $ API.updateTransaction
    { transactionId: tx.transactionId
    , itemIndex: tx.itemIndex
    , actionType: API.ModifyDetails
        { details: newDetails
        }
    }
  H.modify_ _ { tx = updatedTx, isEditingSuggestion = false }
  H.raise $ DetailsUpdated updatedTx
  Console.log $ "[DetailsCell] Updated details for transaction: " <> tx.transactionId <> "#" <> show tx.itemIndex
