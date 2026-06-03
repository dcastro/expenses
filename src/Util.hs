module Util where

import CustomPrelude
import Data.Text qualified as T
import Data.Time qualified as Time
import Effectful
import Effectful.Time qualified as ETime
import Expenses.Effects
import Log
import System.Directory qualified as Dir
import Text.Read qualified as Read

checkDbExists :: (MonadIO m) => (MonadLog m) => FilePath -> m ()
checkDbExists dbPath = do
  unlessM (liftIO $ Dir.doesFileExist dbPath) do
    logAttention_ [i|Database file does not exist: #{dbPath}|]
    exitFailure

eurosToCents :: Text -> Int
eurosToCents txt = do
  let clean = T.replace "," "" $ T.strip txt
      (sign, clean') =
        case T.uncons clean of
          Just ('-', rest) -> (negate, rest)
          Just ('+', rest) -> (id, rest)
          _ -> (id, clean)
      (eurosPart, centsPart) = case T.splitOn "." clean' of
        [e, c] -> (e, T.take 2 $ c <> "00")
        [e] -> (e, "00")
        _ -> ("0", "00")
      euros = Read.read (T.unpack eurosPart) :: Int
      cents = Read.read (T.unpack centsPart) :: Int
      total = euros * 100 + cents
  sign total

timed :: (Time :> es, Log :> es) => Text -> Eff es a -> Eff es a
timed actionName action = do
  start <- ETime.currentTime
  result <- action
  end <- ETime.currentTime
  logTrace_ [i|Finished #{actionName} in: #{Time.diffUTCTime end start}|]
  pure result
