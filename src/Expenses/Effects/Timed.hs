{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Expenses.Effects.Timed where

import CustomPrelude
import Data.Time qualified as Time
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Log

data Timed :: Effect where
  Timed :: Text -> m a -> Timed m a

type instance DispatchOf Timed = Dynamic

timed :: (Timed :> es) => Text -> Eff es a -> Eff es a
timed = send ... Timed

runTimed :: (IOE :> es, Log :> es) => Eff (Timed : es) a -> Eff es a
runTimed = interpret \env -> \case
  Timed actionName action -> do
    localSeqUnlift env \unlift -> do
      start <- liftIO Time.getCurrentTime
      result <- unlift action
      end <- liftIO Time.getCurrentTime
      logTrace_ [i|Finished #{actionName} in: #{Time.diffUTCTime end start}|]
      pure result
