module Expenses.Server.Env where

import Config (AppConfig)
import Control.Lens (classIdFields, makeLensesWith)
import CustomPrelude

data Env = Env
  { eventLogPath :: FilePath
  , logsDir :: FilePath
  , demoMode :: Bool
  , nordigenSecretId :: Text
  , nordigenSecretKey :: Text
  , config :: AppConfig
  }

makeLensesWith classIdFields ''Env
