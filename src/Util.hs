module Util where

import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import CustomPrelude
import Data.Text qualified as T
import Data.Time qualified as Time
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as Db
import Log
import System.Directory qualified as Dir
import Text.Read qualified as Read

checkDbExists :: (MonadIO m) => (MonadLog m) => FilePath -> m ()
checkDbExists dbPath = do
  unlessM (liftIO $ Dir.doesFileExist dbPath) do
    logAttention_ [i|Database file does not exist: #{dbPath}|]
    exitFailure

{-
Lift `Db.withConnection`.

NOTE: we can't use `MonadUnliftIO` because the servant `Handler`s run in `ExceptT`,
and there's no `MonadUnliftIO` instance for `ExceptT`.

```
withConnection :: (MonadUnliftIO m) => FilePath -> (Connection -> m a) -> m a
withConnection dbPath f = do
  withRunInIO $ \runInIO -> do
    Db.withConnection dbPath \conn -> do
      runInIO $ f conn
```

Re-implementing `withConnection` using `bracket` from `safe-exceptions` works.
`bracket` is lifted and requires `MonadMask`:

```
withConnection :: (MonadMask m, MonadIO m) => FilePath -> (Connection -> m a) -> m a
withConnection dbPath f = do
  Safe.bracket (liftIO $ Db.open dbPath) (liftIO . Db.close) \conn -> do
    f conn
```

Ultimately, it's just easier to use `liftBaseOp` from `monad-control`.
-}
withConnection :: (MonadBaseControl IO m) => FilePath -> (Connection -> m a) -> m a
withConnection dbPath = do
  liftBaseOp $ Db.withConnection dbPath

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

timed :: (MonadIO m) => (MonadLog m) => Text -> m a -> m a
timed actionName action = do
  start <- liftIO Time.getCurrentTime
  result <- action
  end <- liftIO Time.getCurrentTime
  logTrace_ [i|Finished #{actionName} in: #{Time.diffUTCTime end start}|]
  pure result
