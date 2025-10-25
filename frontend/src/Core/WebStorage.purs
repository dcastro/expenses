module Core.WebStorage where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as WS

setItem :: forall m. MonadEffect m => String -> String -> m Unit
setItem key value = liftEffect do
  w <- window
  s <- localStorage w
  WS.setItem key value s

getItem :: forall m. MonadEffect m => String -> m (Maybe String)
getItem key = liftEffect do
  w <- window
  s <- localStorage w
  WS.getItem key s
