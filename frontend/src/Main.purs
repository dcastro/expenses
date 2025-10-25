module Main where

import Prelude

import App.Root as Root
import App.Routes as Routes
import Core.API as API
import Core.YearMonth as YM
import Data.Date as Date
import Effect (Effect)
import Effect.Now as Now
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

      now <- H.liftEffect Now.nowDate
      let thisMonth = YM.dateToYearMonth now
      let minMonth = YM.mkYearMonth 2024 Date.January
      transactions <- H.liftAff $ API.getTransactions thisMonth thisMonth
      isAdmin <- H.liftAff API.isAdmin

      allTags <- H.liftAff API.allTags
      allAccounts <- H.liftAff API.allAccounts

      let
        input =
          { thisMonth
          , transactions
          , minMonth
          , isAdmin
          , initRoute: currentRouteOpt
          , allTags
          , allAccounts
          }

      let mainComponent = H.hoist (R.runRouterT router) Root.component
      runUI mainComponent input body
