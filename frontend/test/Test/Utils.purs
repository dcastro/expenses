module Test.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding as Node
import Node.FS.Aff as Node
import Node.FS.Perms as Node
import Node.FS.Sync as NodeSync
import Node.Library.Execa (ExecaResult)
import Node.Library.Execa as Node
import Node.Path (FilePath)
import Node.Path as Path
import Test.Spec.Assertions (fail)
import Yoga.JSON (class WriteForeign)
import Yoga.JSON as J

-- NOTE: when comparing against `Nothing`, we should convert both values to `Nullable a`, otherwise
-- the test will fail with:
--
-- >   The "data" argument must be of type string or an instance of Buffer, TypedArray, or DataView. Received undefined
shouldEqualPretty :: forall a. WriteForeign a => a -> a -> Aff Unit
shouldEqualPretty actual expected = do
  let actualJson = J.writePrettyJSON 2 actual
  let expectedJson = J.writePrettyJSON 2 expected
  mkDirRecursive "temp"
  dir <- Node.mkdtemp "temp/test"
  Aff.finally (cleanup dir) do
    let actualFilePath = Path.concat [ dir, "actual.json" ]
    let expectedFilePath = Path.concat [ dir, "expected.json" ]
    Node.writeTextFile Node.UTF8 actualFilePath actualJson
    Node.writeTextFile Node.UTF8 expectedFilePath expectedJson

    cp <- Node.execa "diff" [ "--ignore-trailing-space", "-u", actualFilePath, expectedFilePath ] identity
    result <- cp.getResult

    when (result.exitCode /= Just 0) do
      fail result.stdout
  where
  cleanup dir = do
    Node.rm' dir { force: false, maxRetries: 100, recursive: true, retryDelay: 1000 }

goldenTest :: forall a. WriteForeign a => FilePath -> a -> Aff Unit
goldenTest expectedFilePath value = do
  -- Serialize the actual value
  let
    actualJson = J.writePrettyJSON 2 value
  -- Check if a snapshot file already exists
  liftEffect (NodeSync.exists expectedFilePath)
    >>= case _ of
      true -> pure unit
      false -> do
        Console.log $ "Snapshot file '" <> expectedFilePath <> "' does not yet exist; creating it..."
        overwriteSnapshotFile actualJson
  result <- shouldEqualFile actualJson expectedFilePath
  when (result.exitCode /= Just 0) do
    -- The snapshot file did not match the expected string.
    -- Update the snapshot file and fail the test.
    overwriteSnapshotFile actualJson
    fail result.stdout
  where
  overwriteSnapshotFile actualJson = do
    -- Create the parent directory if need be
    let dir = Path.dirname expectedFilePath
    mkDirRecursive dir
    Node.writeTextFile Node.UTF8 expectedFilePath (actualJson <> "\n")

-- | Adapted from: https://github.com/jordanmartinez/purescript-spec-golden/blob/v1.0.0/src/Test/Spec/Golden/Assertions.purs#L29-L36
shouldEqualFile :: String -> FilePath -> Aff ExecaResult
shouldEqualFile actual expectedFilePath = do
  cp <- Node.execa "diff" [ "--ignore-trailing-space", "-u", "-", expectedFilePath ] identity
  case cp.stdin of
    Just stdin -> stdin.writeUtf8End actual
    Nothing -> fail "Failed to write `diff`'s stdin"
  result <- cp.getResult
  pure result

mkDirRecursive :: FilePath -> Aff Unit
mkDirRecursive dir =
  unlessM (liftEffect (NodeSync.exists dir)) do
    Node.mkdir' dir { recursive: false, mode: Node.mkPerms Node.all Node.all Node.all }
