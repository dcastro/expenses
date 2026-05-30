module App.Accounts where

import Prelude

import Core.API as API
import Core.APITypes as API
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils

type Slot id = forall query. H.Slot query Output id

type Input =
  { isAdmin :: Boolean
  }

type Output = Void

type State =
  { isAdmin :: Boolean
  , items :: Array API.AccountSyncStatus
  , loading :: Boolean
  , renewing :: Boolean
  }

data Action
  = Initialize
  | RenewRequisition String

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState: \{ isAdmin } ->
        { isAdmin
        , items: []
        , loading: false
        , renewing: false
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.section
    [
    ]
    [ HH.section [ classes' "section is-fullheight" ]
        [ HH.div [ classes' "level" ]
            [ HH.div [ classes' "level-left" ]
                [ HH.h4 [ classes' "title is-4" ] [ HH.text "Accounts" ] ]
            ]
        , if state.loading then
            HH.p [ classes' "has-text-grey" ] [ HH.text "Loading account statuses..." ]
          else
            HH.table [ classes' "table is-fullwidth is-striped" ]
              [ HH.thead []
                  [ HH.tr []
                      [ HH.th [] [ HH.text "Account" ]
                      , HH.th [] [ HH.text "Institution" ]
                      , HH.th [] [ HH.text "Last Sync" ]
                      , HH.th [] [ HH.text "Status" ]
                      , HH.th [] [ HH.text "Transactions" ]
                      , HH.th [] [ HH.text "Error" ]
                      , HH.th [] [ HH.text "Actions" ]
                      ]
                  ]
              , HH.tbody [] (state.items <#> renderRow state)
              ]
        ]
    ]

renderRow :: forall w. State -> API.AccountSyncStatus -> HH.HTML w Action
renderRow state account =
  HH.tr []
    [ HH.td [] [ HH.text account.accountName ]
    , HH.td [ classes' "is-family-monospace" ] [ HH.text account.institutionId ]
    , HH.td [] [ HH.text $ fromMaybe "Never" account.lastSyncFinishedAt ]
    , HH.td [] [ HH.text $ fromMaybe "Never" (showStatus <$> account.lastSyncStatus) ]
    , HH.td [] [ HH.text $ fromMaybe "-" (show <$> account.lastSyncedTransactionCount) ]
    , HH.td [ classes' "is-size-7" ] [ HH.text $ fromMaybe "" account.lastSyncError ]
    , HH.td []
        [ if state.isAdmin then
            HH.button
              [ classes' "button is-small is-primary"
              , HP.disabled (state.loading || state.renewing)
              , HE.onClick \_ -> RenewRequisition account.accountId
              ]
              [ HH.text "Renew" ]
          else
            HH.text ""
        ]
    ]

showStatus :: API.SyncStatus -> String
showStatus = case _ of
  API.SyncSuccess -> "Success"
  API.SyncError -> "Error"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    items <- H.liftAff API.getSyncStatus
    H.modify_ _ { items = items, loading = false }

  RenewRequisition accountId -> do
    H.modify_ _ { renewing = true }
    let redirectUrl = HtmlUtils.apiBaseUrl <> "#/accounts"
    response <- H.liftAff $ API.renewRequisition accountId { redirect: redirectUrl }
    H.liftEffect $ HtmlUtils.redirectTo response.link
