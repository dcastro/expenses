{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{- | Asks Claude a question, with the user's Claude subscription.

We run the Claude Code CLI (`claude -p`) in a child process, and give it the token from
`claude setup-token` in the `CLAUDE_CODE_OAUTH_TOKEN` env var.
See `docs/plans/email-description-suggestions.md`.
-}
module Expenses.Effects.Claude (
  Claude (..),
  askClaude,
  ClaudeRequest (..),
  ClaudeSettings (..),
  ClaudeError (..),
  CliOutput (..),
  runClaude,
  claudeArgs,
  claudeEnv,
  parseCliOutput,
) where

import CustomPrelude
import Data.Aeson (Value (..), eitherDecode)
import Data.Aeson.Encode.Pretty qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text qualified as J
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Log (Log)
import Effectful.TH (makeEffect)
import Log (logInfo_)
import System.Directory qualified as Dir
import System.Environment (getEnvironment)
import System.Process qualified as Process
import System.Process.Typed qualified as P
import System.Timeout qualified as Timeout

data ClaudeRequest = ClaudeRequest
  { model :: Text
  -- ^ The value of `--model`, e.g. "sonnet".
  , systemPrompt :: Text
  , prompt :: Text
  , jsonSchema :: Value
  -- ^ The schema of the response.
  }
  deriving stock (Show, Eq)

data Claude :: Effect where
  -- | Returns Claude's response, which matches the request's `jsonSchema`.
  AskClaude :: ClaudeRequest -> Claude m Value

makeEffect ''Claude

data ClaudeSettings = ClaudeSettings
  { executable :: FilePath
  -- ^ Usually "claude", found in the `PATH`. The tests use a fake one.
  , oauthToken :: Text
  -- ^ The output of `claude setup-token`.
  , workDir :: FilePath
  -- ^ An empty directory, so that Claude Code doesn't find a `CLAUDE.md` or project settings.
  , timeoutSeconds :: Int
  }

newtype ClaudeError = ClaudeError Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The parts of `claude -p --output-format json`'s output that we use.
data CliOutput = CliOutput
  { structuredOutput :: Value
  , durationMs :: Maybe Value
  , usage :: Maybe Value
  }
  deriving stock (Show, Eq)

runClaude :: (IOE :> es, Log :> es) => ClaudeSettings -> Eff (Claude : es) a -> Eff es a
runClaude settings = interpret \_ -> \case
  AskClaude request -> do
    let
      ClaudeRequest{model, prompt} = request
      ClaudeSettings{timeoutSeconds} = settings
    liftIO $ Dir.createDirectoryIfMissing True settings.workDir
    env <- liftIO getEnvironment
    let
      config =
        P.proc settings.executable (claudeArgs request)
          & P.setStdin (P.byteStringInput $ BSL.fromStrict $ encodeUtf8 request.prompt)
          & P.setWorkingDir settings.workDir
          & P.setEnv (claudeEnv settings.oauthToken env)
          & P.setStdout P.byteStringOutput
          & P.setStderr P.byteStringOutput

    logInfo_ [i|[Claude] Asking Claude (model: #{model}, prompt: #{T.length prompt} chars).|]
    result <- liftIO $ P.withProcessTerm config \process -> do
      result <-
        Timeout.timeout (timeoutSeconds * 1_000_000)
          $ atomically
          $ (,,)
          <$> P.waitExitCodeSTM process
          <*> P.getStdout process
          <*> P.getStderr process
      {- NOTE: on a timeout, we must send SIGTERM ourselves.
      `withProcessTerm`'s cleanup only sends it after it cancels the thread that waits for the process.
      With the threaded runtime, that thread is blocked in a foreign call (`waitForProcess`),
      so cancelling it would wait until `claude` exits on its own.
      -}
      when (isNothing result) do
        Process.terminateProcess (P.unsafeProcessHandle process)
      pure result
    case result of
      Nothing ->
        throwM $ ClaudeError [i|`claude` did not finish within #{timeoutSeconds} seconds.|]
      Just (P.ExitFailure code, stdout, stderr) -> do
        -- E.g. with an invalid token, `claude` exits with 1, and the reason is in stdout's JSON (`is_error: true`).
        let reason = either id (\_ -> cut stdout) (parseCliOutput stdout)
        throwM $ ClaudeError [i|`claude` exited with code #{code}: #{reason} #{cut stderr}|]
      Just (P.ExitSuccess, stdout, _) ->
        case parseCliOutput stdout of
          Left err -> throwM $ ClaudeError err
          Right CliOutput{structuredOutput, durationMs, usage} -> do
            logInfo_ [i|[Claude] Done. Duration (ms): #{renderMaybe durationMs}. Usage: #{renderMaybe usage}|]
            pure structuredOutput
 where
  cut :: LByteString -> Text
  cut = T.take 2000 . decodeUtf8 . BSL.toStrict

  renderMaybe :: Maybe Value -> Text
  renderMaybe = maybe "?" (toStrict . J.encodeToLazyText)

{- | The CLI arguments.

  * `--tools ""`: Claude gets no tools. An e-mail can try to give Claude instructions (prompt injection).
    Without tools, the worst outcome is a bad suggestion, which the user must approve anyway.
  * `--setting-sources ""`, `--strict-mcp-config`, `--no-session-persistence`: ignore the machine's
    Claude Code settings and MCP servers, and don't save the session.
-}
claudeArgs :: ClaudeRequest -> [String]
claudeArgs request =
  [ "-p"
  , "--model"
  , toString request.model
  , "--output-format"
  , "json"
  , "--json-schema"
  , toString $ decodeUtf8 @Text $ BSL.toStrict $ J.encodePretty' J.defConfig{J.confIndent = J.Spaces 0} request.jsonSchema
  , "--system-prompt"
  , toString request.systemPrompt
  , "--tools"
  , ""
  , "--setting-sources"
  , ""
  , "--strict-mcp-config"
  , "--no-session-persistence"
  ]

{- | The env of the child process: the server's env, plus the subscription token.

We remove `ANTHROPIC_API_KEY`, so that Claude Code always uses the subscription
and never bills an API key.
-}
claudeEnv :: Text -> [(String, String)] -> [(String, String)]
claudeEnv oauthToken env =
  filter (\(name, _) -> name `notElem` overridden) env
    <> [ ("CLAUDE_CODE_OAUTH_TOKEN", toString oauthToken)
       , ("CLAUDE_CODE_DISABLE_AUTO_MEMORY", "1")
       , ("DISABLE_AUTOUPDATER", "1")
       ]
 where
  overridden =
    [ "ANTHROPIC_API_KEY"
    , "CLAUDE_CODE_OAUTH_TOKEN"
    , "CLAUDE_CODE_DISABLE_AUTO_MEMORY"
    , "DISABLE_AUTOUPDATER"
    ]

-- | Parses the stdout of `claude -p --output-format json --json-schema ...`.
parseCliOutput :: LByteString -> Either Text CliOutput
parseCliOutput stdout =
  case eitherDecode @Value stdout of
    Left err -> Left [i|Failed to decode the output of `claude`: #{err}|]
    Right (Object o)
      | KM.lookup "is_error" o == Just (Bool True) ->
          Left [i|`claude` returned an error: #{maybe "?" (toStrict . J.encodeToLazyText) (KM.lookup "result" o)}|]
      | otherwise ->
          case KM.lookup "structured_output" o of
            Nothing -> Left "The output of `claude` has no `structured_output`."
            Just Null -> Left "The output of `claude` has no `structured_output`."
            Just structuredOutput ->
              Right
                CliOutput
                  { structuredOutput
                  , durationMs = KM.lookup "duration_ms" o
                  , usage = KM.lookup "usage" o
                  }
    Right _ -> Left "The output of `claude` is not a JSON object."
