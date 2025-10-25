module App.NewTransactionModal.AmountInput
  ( sanitizeAmountInput
  , tryParseAmountCents
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Tuple.Nested ((/\))

tryParseAmountCents :: String -> Either String Int
tryParseAmountCents str = do

  str <- pure $ Str.trim str

  (hasSign /\ withoutSign) <-
    case Str.stripPrefix (Pattern "-") str of
      Just withoutSign -> Right (true /\ withoutSign)
      Nothing -> Right (false /\ str)

  (whole /\ decimal) <- case Str.split (Pattern ".") withoutSign of
    [] -> Left genericError -- impossible, `split` should never return an empty array
    [ whole ] -> Right (whole /\ Nothing)
    [ whole, decimal ] -> Right (whole /\ Just decimal)
    _ -> Left "Please enter at most one decimal point."

  euros <- case Int.fromString whole of
    Just n -> Right n
    Nothing -> Left genericError

  cents <- case decimal of
    Just decimalStr | Str.length decimalStr /= 2 ->
      -- If the user entered a decimal part, we force them to enter exactly two digits.
      -- This avoids ambiguity about whether "12.3" means "12.30" or "12.03".
      Left "Please enter two decimal places."
    Just decimalStr -> case Int.fromString decimalStr of
      Nothing -> Left genericError
      Just n -> Right n
    Nothing -> Right 0

  -- We've already stripped the `-` sign, so both euros and cents should be non-negative.
  when (euros < 0 || cents < 0) do
    Left genericError

  let amount = (euros * 100 + cents) * (if hasSign then -1 else 1)

  when (amount == 0) do
    Left "Amount cannot be zero."

  pure amount

  where
  genericError = "Invalid amount."

sanitizeAmountInput :: String -> String
sanitizeAmountInput = do
  Str.replaceAll (Pattern ",") (Replacement ".")
