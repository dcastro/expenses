module Choices where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

foreign import init :: String -> Effect Foreign

foreign import selectValue :: Foreign -> String -> Effect Unit

foreign import clearSelection :: Foreign -> Effect Unit
