module Test.App.NewTransactionModalSpec where

import Prelude

import App.NewTransactionModal.AmountInput (tryParseAmountCents)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec =
  describe "tryParseAmountCents" do
    it "parses correctly " do
      "" `shouldParse` Left "Invalid amount."
      " " `shouldParse` Left "Invalid amount."
      "-" `shouldParse` Left "Invalid amount."
      "." `shouldParse` Left "Invalid amount."
      ".." `shouldParse` Left "Please enter at most one decimal point."
      ",," `shouldParse` Left "Invalid amount."
      "5" `shouldParse` Right 500
      "57" `shouldParse` Right 5700
      "123" `shouldParse` Right 12300
      "-5" `shouldParse` Right (-500)
      "-57" `shouldParse` Right (-5700)
      "-123" `shouldParse` Right (-12300)
      "-00123" `shouldParse` Right (-12300)
      -- decimal places
      ".12" `shouldParse` Left "Invalid amount."
      "-.12" `shouldParse` Left "Invalid amount."
      "12." `shouldParse` Left "Please enter two decimal places."
      "12.3" `shouldParse` Left "Please enter two decimal places."
      "12.333" `shouldParse` Left "Please enter two decimal places."
      "-12." `shouldParse` Left "Please enter two decimal places."
      "-12.3" `shouldParse` Left "Please enter two decimal places."
      "-12.333" `shouldParse` Left "Please enter two decimal places."
      "12.00" `shouldParse` Right 1200
      "12.34" `shouldParse` Right 1234
      "0.34" `shouldParse` Right 34
      "000.34" `shouldParse` Right 34
      "-12.00" `shouldParse` Right (-1200)
      "-12.34" `shouldParse` Right (-1234)
      "-0.34" `shouldParse` Right (-34)
      "-000.34" `shouldParse` Right (-34)
      -- whitespace
      "  5  " `shouldParse` Right 500
      "  5.30  " `shouldParse` Right 530
      "  -5.30  " `shouldParse` Right (-530)
      "  - 5.30  " `shouldParse` Left "Invalid amount."
      -- invalid characters
      "a" `shouldParse` Left "Invalid amount."
      "12.b6" `shouldParse` Left "Invalid amount."
      -- additional `-` signs
      "--1.23" `shouldParse` Left "Invalid amount."
      "-1.-2" `shouldParse` Left "Invalid amount."
      "1.-2" `shouldParse` Left "Invalid amount."

shouldParse :: forall m. MonadThrow Error m => String -> Either String Int -> m Unit
shouldParse str expected = do
  let result = tryParseAmountCents str
  when (result /= expected)
    $ fail
    $ "Input: '" <> str <> "'\n"
        <> show result
        <> " ≠ "
        <> show expected
