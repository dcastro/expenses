module Main where

import Prelude

import App.Root as Root
import App.Routes as Routes
import Core.API as API
import Core.YearMonth as YM
import Data.Date as Date
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Router.Class as R
import Halogen.Router.Trans.Hash as R
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff
    do
      body <- HA.awaitBody

      -- Get the initial route from the URL
      router <- H.liftEffect $ R.mkRouter Routes.routeCodec
      currentRouteOpt <- R.runRouterT router R.current

      dates <- H.liftAff API.getDates
      let minMonth = max dates.minMonth (YM.mkYearMonth 2024 Date.January)
      let maxMonth = dates.maxMonth
      transactions <- H.liftAff $ API.getTransactions maxMonth maxMonth
      isAdmin <- H.liftAff API.isAdmin

      allTags <- H.liftAff API.allTags
      allAccounts <- H.liftAff API.allAccounts

      let
        input =
          { maxMonth
          , transactions
          , minMonth
          , isAdmin
          , initRoute: currentRouteOpt
          , allTags
          , allAccounts
          }

      let mainComponent = H.hoist (R.runRouterT router) Root.component
      runUI mainComponent input body
