module App.Accounts where

import Prelude

import App.Routes as Routes
import Core.API as API
import Core.APITypes as API
import Data.Array as Arr
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
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
  -- Accounts that the user has connected to, but are missing from their config.
  , missingAccounts :: Array API.MissingInstitutionAccounts
  , loading :: Boolean -- Whether the account statuses are currently being loaded from the server
  , renewing :: Boolean -- Whether a requisition renewal is currently in progress
  , syncing :: Boolean -- Whether we're currently syncing accounts
  -- Set of account IDs for which the sync error details are currently expanded in the UI
  , expandedErrors :: Set String
  }

data Action
  = Initialize
  | RenewRequisition String
  | SyncNow
  | ToggleError String

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState: \{ isAdmin } ->
        { isAdmin
        , institutions: []
        , missingAccounts: []
        , loading: false
        , renewing: false
        , syncing: false
        , expandedErrors: Set.empty
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.section []
    [ HH.section [ classes' "section is-fullheight" ]
        [ HH.h4 [ classes' "title is-4 has-text-centered" ]
            [ HH.text "Accounts" ]
        , renderMissingAccountsWarning state
        , if state.loading then
            HH.p [ classes' "has-text-grey" ] [ HH.text "Loading account statuses..." ]
          else
            HH.div [] (state.institutions <#> renderInstitution state)
        , if state.isAdmin then
            HH.div [ classes' "mt-4" ]
              [ HH.button
                  [ classes' $ "button is-primary" # HtmlUtils.addClassIf state.syncing "is-loading"
                  , HP.disabled (state.loading || state.renewing || state.syncing)
                  , HE.onClick \_ -> SyncNow
                  ]
                  [ HH.text "Sync now" ]
              ]
          else HH.text ""
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
      -- NOTE: the `content` class sets the style for the `ul` lists.
      , HH.div [ classes' "message-body content" ]
          [ HH.p [] [ HH.text "You have connected to these institutions, but some accounts are missing from your config:" ]
          , HH.ul [] (state.missingAccounts <#> renderMissingInstitution)
          ]
      ]
  where
  renderMissingInstitution :: API.MissingInstitutionAccounts -> HH.HTML w Action
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
  HH.div [ classes' "box" ]
    -- NOTE: `is-justify-content-space-between`: put the "Renew" button on the right, separated from the institution name and requisition status tag.
    [ HH.div [ classes' "is-flex is-justify-content-space-between mb-3" ]
        [ HH.div
            -- NOTE: `is-flex` on this div allows the institution name and requisition status tag to be on the same line,
            -- and for the institution name to take up all available space while the tag takes up only as much space as it needs.
            -- NOTE: `min-width: 0` is necessary for the institution name to
            -- properly truncate with an ellipsis when it's too long for the available space.
            [ classes' "is-flex"
            , HP.style "min-width: 0; flex: 1; margin-right: 1rem"
            ]
            [ HH.span
                [ classes' "has-text-weight-semibold is-size-6 is-family-monospace mr-2"
                , HP.style "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; min-width: 0"
                , HP.title institution.institutionId
                ]
                [ HH.text institution.institutionId ]
            , HH.span
                [ classes' $ "tag " <> requisitionStatusTagClass institution.requisitionStatus
                , HP.style "flex-shrink: 0"
                ]
                [ HH.text $ fromMaybe "UNKNOWN" institution.requisitionStatus ]
            ]
        , if state.isAdmin then
            HH.button
              [ classes' "button is-small"
              , HP.style "flex-shrink: 0"
              , HP.disabled (state.loading || state.renewing || state.syncing)
              , HE.onClick \_ -> RenewRequisition institution.institutionId
              ]
              [ HH.text "Renew" ]
          else
            HH.text ""
        ]
    , HH.div [] $ Arr.intersperse (HH.hr [ classes' "my-2" ])
        (institution.accountStatuses <#> renderAccount state)
    ]

requisitionStatusTagClass :: Maybe String -> String
requisitionStatusTagClass = case _ of
  Just "LINKED" -> "is-success"
  Just "EXPIRED" -> "is-danger"
  Just "REJECTED" -> "is-danger"
  Just _ -> "is-warning"
  Nothing -> "is-light"

renderAccount :: forall w. State -> API.AccountSyncStatus -> HH.HTML w Action
renderAccount state account =
  HH.div []
    [ HH.div [ classes' "is-flex is-justify-content-space-between is-align-items-center" ]
        [ HH.span []
            [ HH.text account.accountName
            , HH.span [ classes' "is-family-monospace is-size-7 has-text-grey ml-2" ]
                [ HH.text $ "(" <> account.accountId <> ")" ]
            ]
        , renderSyncStatus state account
        ]
    , case account.lastSyncError of
        Just err | Set.member account.accountId state.expandedErrors ->
          HH.p [ classes' "is-size-7 has-text-danger mt-1" ]
            [ HH.pre [] [ HH.text err ] ]
        _ -> HH.text ""
    ]

renderSyncStatus :: forall w. State -> API.AccountSyncStatus -> HH.HTML w Action
renderSyncStatus state account =
  case account.lastSyncStatus of
    Nothing ->
      HH.span [ classes' "has-text-grey is-size-7" ] [ HH.text "Never synced" ]
    Just API.SyncSuccess ->
      HH.span [ classes' "tag is-success is-light" ] [ HH.text "Success" ]
    Just API.SyncError ->
      HH.span
        [ classes' "tag is-danger is-light is-clickable"
        , HE.onClick \_ -> ToggleError account.accountId
        ]
        [ HH.text $
            if Set.member account.accountId state.expandedErrors then "Error ▲"
            else "Error ▼"
        ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    H.modify_ _ { loading = true }
    syncStatus <- H.liftAff API.getSyncAccountStatus
    H.modify_ _ { institutions = syncStatus.institutions, missingAccounts = syncStatus.missingAccounts, loading = false }

  RenewRequisition institutionId -> do
    H.modify_ _ { renewing = true }
    baseUrl <- H.liftEffect HtmlUtils.apiBaseUrl
    let redirectUrl = baseUrl <> "#" <> (RouteDuplex.print Routes.routeCodec (Routes.Accounts Routes.defaultModalFlag))
    response <- H.liftAff $ API.renewRequisition institutionId { redirect: redirectUrl }
    H.liftEffect $ HtmlUtils.redirectTo response.link

  SyncNow -> do
    H.modify_ _ { syncing = true }
    H.liftAff API.triggerSync
    syncStatus <- H.liftAff API.getSyncAccountStatus
    H.modify_ _ { institutions = syncStatus.institutions, missingAccounts = syncStatus.missingAccounts, syncing = false }

  ToggleError accountId ->
    H.modify_ \s -> s
      { expandedErrors =
          if Set.member accountId s.expandedErrors then Set.delete accountId s.expandedErrors
          else Set.insert accountId s.expandedErrors
      }
