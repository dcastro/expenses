module Expenses.Server.Options where

import CustomPrelude
import Data.Text qualified as T
import Log (MonadLog, logAttention_)
import Log.Class (logInfo_)
import Options.Applicative
import System.Directory qualified as Dir
import System.Environment (getEnv)
import System.Environment qualified as Env
import System.FilePath ((</>))
import System.FilePath qualified as Path
import Types (Username (..), mkUsername)

data RawServerOptions = RawServerOptions
  { port :: Int
  , appDir :: Maybe FilePath
  , runCron :: Bool
  , user :: Maybe Username
  , isVerbose :: Bool
  }
  deriving stock (Show, Generic)

data ServerOptions = ServerOptions
  { port :: Int
  , dbPath :: FilePath
  , eventLogPath :: FilePath
  , runCron :: Bool
  , user :: Maybe Username
  , resourcesDir :: FilePath
  , logsDir :: FilePath
  , isVerbose :: Bool
  , nordigenSecretId :: Text
  , nordigenSecretKey :: Text
  }
  deriving stock (Show, Generic)

mkServerOptions :: (MonadIO m, MonadLog m) => m ServerOptions
mkServerOptions = do
  nordigenSecretId <- liftIO $ getEnv "EXPENSES_NORDIGEN_SECRET_ID"
  nordigenSecretKey <- liftIO $ getEnv "EXPENSES_NORDIGEN_SECRET_KEY"
  RawServerOptions{port, appDir, runCron, user, isVerbose} <- liftIO parseRawServerOptions

  let
    dbPath = appDir <&> (</> "expenses.db") & fromMaybe "db/db"
    eventLogPath = appDir <&> (</> "eventlog.jsonl") & fromMaybe "logs/eventlog.jsonl"
    logsDir = appDir <&> (</> "logs") & fromMaybe "logs"

  dbPath <- liftIO $ Dir.canonicalizePath dbPath
  eventLogPath <- liftIO $ Dir.canonicalizePath eventLogPath
  logsDir <- liftIO $ Dir.canonicalizePath logsDir

  liftIO $ Dir.createDirectoryIfMissing True logsDir

  resourcesDir <- getResourcesDir

  let fallbackUserLog = maybe "None" (\u -> u.unUsername.getNonEmptyText) user
  logInfo_ [i|----------------------------------------------------------------------|]
  logInfo_ [i|Starting server on port #{port}|]
  logInfo_ [i|Fallback user: #{fallbackUserLog}|]
  logInfo_ [i|Cron job enabled: #{runCron}|]
  logInfo_ [i|DB path: #{dbPath}|]
  logInfo_ [i|Event log path: #{eventLogPath}|]
  logInfo_ [i|Logs: #{logsDir}|]
  logInfo_ [i|Verbose logging: #{isVerbose}|]
  logInfo_ [i|Static resources: #{resourcesDir}|]
  logInfo_ [i|----------------------------------------------------------------------|]

  pure
    ServerOptions
      { port
      , dbPath
      , eventLogPath
      , runCron
      , user
      , resourcesDir
      , logsDir
      , isVerbose
      , nordigenSecretId = T.pack nordigenSecretId
      , nordigenSecretKey = T.pack nordigenSecretKey
      }

parseRawServerOptions :: IO RawServerOptions
parseRawServerOptions = execParser opts
 where
  opts =
    info
      (serverOptionsParser <**> helper)
      ( fullDesc
          <> progDesc "Run the expenses server"
          <> header "expenses-server - a simple expenses server"
      )

  serverOptionsParser :: Parser RawServerOptions
  serverOptionsParser =
    RawServerOptions
      <$> option
        auto
        ( long "port"
            <> metavar "PORT"
            <> help "Port to run the server on"
            <> showDefault
            <> value 8081
        )
      <*> optional
        ( strOption
            ( long "app-dir"
                <> metavar "DIR"
                <> help "Base directory for the application data files"
            )
        )
      <*> switch
        ( long "cron"
            <> help "Run the Nordigen cron job"
        )
      <*> optional
        ( option
            (eitherReader parseUsernameOption)
            ( long "user"
                <> metavar "USERNAME"
                <> help "Fallback username when the auth header is missing"
            )
        )
      <*> switch
        ( long "verbose"
            <> help "Enable verbose logging"
        )

  parseUsernameOption :: String -> Either String Username
  parseUsernameOption raw = do
    let input = T.pack raw
    mkUsername input
      & maybe (Left "--user must be a non-empty username") Right

getResourcesDir :: (MonadLog m, MonadIO m) => m FilePath
getResourcesDir = do
  exeDir <- liftIO Env.getExecutablePath <&> (\p -> Path.takeDirectory p </> "resources")
  workingDir <- liftIO Dir.getCurrentDirectory <&> (</> "frontend" </> "prod")

  {- If we're running from the nix store, we should be able to find the static files relative to the executable directory.

            $ nix build .#expenses-manager-bundle-native ; tree result
            result
            └── bin
                ├── expenses-manager-server
                └── resources
                    ├── bundle.2f098c4a.js
                    ├── bundle.2f098c4a.js.map
                    └── index.html
  -}
  ifM
    (liftIO $ Dir.doesDirectoryExist exeDir)
    do
      pure exeDir
    ( -- If we're running in "dev mode", we should be able to find the static files relative to the project's working directory.
      ifM
        (liftIO $ Dir.doesDirectoryExist workingDir)
        do
          pure workingDir
        do
          logAttention_
            [i|
            Failed to find resources directory in:
              * #{exeDir}
              * #{workingDir}
          |]
          exitFailure
    )
