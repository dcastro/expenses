{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.EventLog where

import CustomPrelude
import Data.Aeson.Encoding qualified as J
import Data.ByteString.Lazy qualified as BSL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Expenses.Server.Env (Env (..))
import Expenses.Server.EventLog (Action)
import Expenses.Server.EventLog qualified as EL

data EventLog :: Effect where
  AppendEvent :: Action -> EventLog m ()

type instance DispatchOf EventLog = Dynamic

runEventLog :: (Reader Env :> es, IOE :> es) => Eff (EventLog : es) a -> Eff es a
runEventLog = interpret \_ -> \case
  AppendEvent action -> do
    env <- ask @Env
    let bs = J.encodingToLazyByteString $ EL.encoding action
    liftIO $ BSL.appendFile env.eventLogPath (bs <> "\n")

appendEvent :: (EventLog :> es) => Action -> Eff es ()
appendEvent = send . AppendEvent
