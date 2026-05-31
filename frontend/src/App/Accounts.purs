module App.Accounts where

import Prelude

import App.Routes as Routes
import Core.API as API
import Core.APITypes as API
import Data.Array as Arr
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (classes')
import HtmlUtils as HtmlUtils
import Routing.Duplex as RouteDuplex

type Slot id = forall query. H.Slot query Output id

type Input =
  { isAdmin :: Boolean
  }

type Output = Void

type State =
  { isAdmin :: Boolean
  , institutions :: Array API.InstitutionSyncStatus
  , missingAccounts :: Array API.MissingInstitutionAccounts
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
        , institutions: []
        , missingAccounts: []
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
        [ HH.h4 [ classes' "title is-4 has-text-centered" ]
            [ HH.text "Accounts" ]
        , renderMissingAccountsWarning state
        , if state.loading then
            HH.p [ classes' "has-text-grey" ] [ HH.text "Loading account statuses..." ]
          else
            HH.div [] (state.institutions <#> renderInstitution state)
        ]
    ]

renderMissingAccountsWarning :: forall w. State -> HH.HTML w Action
renderMissingAccountsWarning state =
  if Arr.null state.missingAccounts then
    HH.text ""
  else
    HH.article [ classes' "message is-warning" ]
      [ HH.div [ classes' "message-header" ]
          [ HH.text "Missing configured accounts" ]
      , HH.div [ classes' "message-body" ]
          [ HH.p [] [ HH.text "Some accounts are enabled in Nordigen requisitions but not present in config. Add these account IDs to config:" ]
          , HH.ul [] (state.missingAccounts <#> renderMissingInstitution)
          ]
      ]

renderMissingInstitution :: forall w. API.MissingInstitutionAccounts -> HH.HTML w Action
renderMissingInstitution missingInstitution =
  HH.li []
    [ HH.p []
        [ HH.text "Institution: "
        , HH.span [ classes' "is-family-monospace" ] [ HH.text missingInstitution.institutionName ]
        ]
    , HH.ul []
        ( missingInstitution.missingAccountIds
            <#> \accountId ->
              HH.li [ classes' "is-family-monospace" ] [ HH.text accountId ]
        )
    ]

renderInstitution :: forall w. State -> API.InstitutionSyncStatus -> HH.HTML w Action
renderInstitution state institution =
  HH.div [ classes' "mb-5" ]
    [ HH.div [ classes' "is-flex is-justify-content-space-between is-align-items-center mb-2" ]
        [ HH.h5 [ classes' "title is-5 mb-0" ]
            [ HH.text "Institution: "
            , HH.span [ classes' "is-family-monospace" ] [ HH.text institution.institutionId ]
            ]
        , if state.isAdmin then
            HH.button
              [ classes' "button is-small is-primary"
              , HP.disabled (state.loading || state.renewing)
              , HE.onClick \_ -> RenewRequisition institution.institutionId
              ]
              [ HH.text "Renew" ]
          else
            HH.text ""
        ]
    , HH.div [ classes' "table-container" ]
        [ HH.table [ classes' "table is-fullwidth is-striped" ]
            [ HH.thead []
                [ HH.tr []
                    [ HH.th [] [ HH.text "Account" ]
                    , HH.th [] [ HH.text "Account ID" ]
                    , HH.th [] [ HH.text "Requisition" ]
                    , HH.th [] [ HH.text "Last Sync Status" ]
                    , HH.th [] [ HH.text "Error" ]
                    ]
                ]
            , HH.tbody [] (institution.accountStatuses <#> renderAccountRow)
            ]
        ]
    ]

renderAccountRow :: forall w. API.AccountSyncStatus -> HH.HTML w Action
renderAccountRow account =
  HH.tr []
    [ HH.td [] [ HH.text account.accountName ]
    , HH.td [ classes' "is-family-monospace" ] [ HH.text account.accountId ]
    , HH.td [ classes' "is-family-monospace" ] [ HH.text $ fromMaybe "-" account.requisitionStatus ]
    , HH.td [] [ HH.text $ fromMaybe "Never" (showStatus <$> account.lastSyncStatus) ]
    , HH.td [ classes' "is-size-7" ] [ HH.text $ fromMaybe "" account.lastSyncError ]
    ]

showStatus :: API.SyncStatus -> String
showStatus = case _ of
  API.SyncSuccess -> "Success"
  API.SyncError -> "Error"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    syncStatus <- H.liftAff API.getSyncAccountStatus
    H.modify_ _ { institutions = syncStatus.institutions, missingAccounts = syncStatus.missingAccounts, loading = false }

  RenewRequisition institutionId -> do
    H.modify_ _ { renewing = true }
    let redirectUrl = HtmlUtils.apiBaseUrl <> "#" <> (RouteDuplex.print Routes.routeCodec (Routes.Accounts Routes.defaultModalFlag))
    response <- H.liftAff $ API.renewRequisition institutionId { redirect: redirectUrl }
    H.liftEffect $ HtmlUtils.redirectTo response.link
