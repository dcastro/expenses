module Expenses.Effects.ClaudeSpec where

import CustomPrelude
import Data.Aeson (Value, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import Effectful.Log (runLog)
import Expenses.Effects.Claude
import Expenses.Test.Util qualified as Util
import Log (LogLevel (..))
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "parseCliOutput" do
    it "returns the structured output" do
      fixture <- BSL.readFile "test/fixtures/claude/success.json"
      expected <- Util.readStructuredOutput "test/fixtures/claude/success.json"
      (parseCliOutput fixture <&> (.structuredOutput)) `shouldBe` Right expected

    it "returns the error message when `is_error` is true" do
      fixture <- BSL.readFile "test/fixtures/claude/invalid-token.json"
      parseCliOutput fixture
        `shouldBe` Left "`claude` returned an error: \"Failed to authenticate. API Error: 401 Invalid bearer token\""

    it "fails when the output is not JSON" do
      parseCliOutput "Something went wrong" `shouldSatisfy` isLeft

  describe "claudeEnv" do
    it "sets the token, and removes the API key" do
      claudeEnv "new-token" [("PATH", "/bin"), ("ANTHROPIC_API_KEY", "sk-ant"), ("CLAUDE_CODE_OAUTH_TOKEN", "old-token")]
        `shouldBe` [ ("PATH", "/bin")
                   , ("CLAUDE_CODE_OAUTH_TOKEN", "new-token")
                   , ("CLAUDE_CODE_DISABLE_AUTO_MEMORY", "1")
                   , ("DISABLE_AUTOUPDATER", "1")
                   ]

  describe "claudeArgs" do
    it "gives Claude no tools" do
      let args = claudeArgs request
      dropWhile (/= "--tools") args `shouldSatisfy` \case
        "--tools" : "" : _ -> True
        _ -> False

  describe "runClaude" do
    it "sends the prompt on stdin, the token in the env, and returns the structured output" do
      withSystemTempDirectory "claude-spec" \tmp -> do
        fixture <- Dir.makeAbsolute "test/fixtures/claude/success.json"
        exe <- writeFakeClaude tmp [i|cat > "#{tmp}/stdin.txt"; printf '%s' "$CLAUDE_CODE_OAUTH_TOKEN" > "#{tmp}/token.txt"; pwd > "#{tmp}/pwd.txt"; cat "#{fixture}"|]
        result <- ask' (settings exe tmp 10)

        expected <- Util.readStructuredOutput fixture
        result `shouldBe` Right expected
        TIO.readFile (tmp </> "stdin.txt") >>= (`shouldBe` request.prompt)
        TIO.readFile (tmp </> "token.txt") >>= (`shouldBe` "test-token")
        TIO.readFile (tmp </> "pwd.txt") >>= (`shouldBe` toText (tmp </> "workdir") <> "\n")

    it "throws with the reason from stdout when `claude` fails" do
      withSystemTempDirectory "claude-spec" \tmp -> do
        fixture <- Dir.makeAbsolute "test/fixtures/claude/invalid-token.json"
        exe <- writeFakeClaude tmp [i|cat > /dev/null; cat "#{fixture}"; exit 1|]
        result <- ask' (settings exe tmp 10)
        case result of
          Left err -> err `shouldSatisfy` T.isInfixOf "exited with code 1: `claude` returned an error: \"Failed to authenticate. API Error: 401 Invalid bearer token\""
          Right _ -> expectationFailure "Expected an error"

    it "throws when `claude` takes longer than the timeout" do
      withSystemTempDirectory "claude-spec" \tmp -> do
        -- NOTE: `exec`, so that the fake is a single process, like the real `claude`.
        -- Without it, SIGTERM goes to bash, which only exits after `sleep` does.
        exe <- writeFakeClaude tmp "exec sleep 30"
        result <- ask' (settings exe tmp 1)
        result `shouldBe` Left "`claude` did not finish within 1 seconds."
 where
  settings :: FilePath -> FilePath -> Int -> ClaudeSettings
  settings exe tmp timeoutSeconds =
    ClaudeSettings
      { executable = exe
      , oauthToken = "test-token"
      , workDir = tmp </> "workdir"
      , timeoutSeconds
      }

  -- Runs `askClaude`, and catches `ClaudeError`.
  ask' :: ClaudeSettings -> IO (Either Text Value)
  ask' s =
    try @_ @ClaudeError (askClaude request & runClaude s & runLog "test" mempty LogAttention & runEff)
      <&> first (\(ClaudeError err) -> err)

request :: ClaudeRequest
request =
  ClaudeRequest
    { model = "sonnet"
    , systemPrompt = "You match transactions with emails."
    , prompt = "# Transactions\n\n| tx-1 | 19.99 |\n"
    , jsonSchema = object ["type" .= ("object" :: Text)]
    }

-- | Writes an executable bash script that stands in for `claude`.
writeFakeClaude :: FilePath -> Text -> IO FilePath
writeFakeClaude dir script = do
  let path = dir </> "fake-claude"
  TIO.writeFile path ("#!/usr/bin/env bash\n" <> script <> "\n")
  perms <- Dir.getPermissions path
  Dir.setPermissions path (Dir.setOwnerExecutable True perms)
  pure path
