module Core.YearMonth where

import Prelude

import Data.Argonaut (class DecodeJson)
import Data.Argonaut as J
import Data.Date (Date, Month, Year)
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (class Enum, fromEnum, pred, succ, toEnum)
import Data.Formatter.DateTime as F
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen.HTML (class IsProp)
import Halogen.HTML.Core (toPropValue)
import Utils (unsafeFromJust, unsafeFromRight)

data YearMonth = YearMonth
  { year :: Year
  , month :: Month
  }

instance IsProp YearMonth where
  toPropValue = formatYearMonth >>> toPropValue

instance Show YearMonth where
  show = formatYearMonth

instance DecodeJson YearMonth where
  decodeJson json = do
    (str :: String) <- J.decodeJson json
    case parseYearMonth str of
      Just ym -> Right ym
      Nothing -> Left $ J.UnexpectedValue json

derive instance Eq YearMonth

instance Ord YearMonth where
  compare (YearMonth { year: y1, month: m1 }) (YearMonth { year: y2, month: m2 }) =
    compare y1 y2 <> compare m1 m2

instance Enum YearMonth where
  succ (YearMonth { year, month }) =
    Just $ case succ month of
      Just newMonth -> YearMonth { year, month: newMonth }
      Nothing ->
        YearMonth { year: succ year # unsafeFromJust "impossible: failed to increment year", month: bottom }
  pred (YearMonth { year, month }) =
    Just $ case pred month of
      Just newMonth -> YearMonth { year, month: newMonth }
      Nothing ->
        YearMonth { year: pred year # unsafeFromJust "impossible: failed to decrement year", month: top }

mkYearMonth :: Int -> Month -> YearMonth
mkYearMonth year month =
  YearMonth
    { year: toEnum year # unsafeFromJust "impossible: invalid year"
    , month
    }

dateToYearMonth :: Date -> YearMonth
dateToYearMonth date =
  YearMonth
    { year: Date.year date
    , month: Date.month date
    }

formatYearMonth :: YearMonth -> String
formatYearMonth ym = do
  unsafeFromRight $ F.formatDateTime "YYYY-MM" (yearMonthToDateTime ym)

formatYearMonthPretty :: YearMonth -> String
formatYearMonthPretty ym = do
  unsafeFromRight $ F.formatDateTime "MMM YYYY" (yearMonthToDateTime ym)

yearMonthToDateTime :: YearMonth -> DateTime
yearMonthToDateTime (YearMonth { year, month }) =
  DateTime
    ( Date.canonicalDate
        year
        month
        (toEnum 1 # unsafeFromJust "impossible: failed to parse day 1")
    )
    bottom

-- Parse a string in YYYY-MM format to YearMonth
parseYearMonth :: String -> Maybe YearMonth
parseYearMonth str =
  case String.split (String.Pattern "-") str of
    [ yearStr, monthStr ] ->
      let
        my = Int.fromString yearStr >>= toEnum
        mm = Int.fromString monthStr >>= toEnum
      in
        case my, mm of
          Just year, Just month -> Just (YearMonth { year, month })
          _, _ -> Nothing
    _ -> Nothing

nextMonth :: YearMonth -> YearMonth -> Maybe YearMonth
nextMonth ym upperBound =
  if ym == upperBound then Nothing
  else succ ym

prevMonth :: YearMonth -> YearMonth -> Maybe YearMonth
prevMonth ym lowerBound =
  if ym == lowerBound then Nothing
  else pred ym

-- | The number of months passed between 2 dates, with inclusive bounds.
-- Assumes `ym2 >= ym1`.
monthsSpan :: YearMonth -> YearMonth -> Int
monthsSpan ym1 ym2 =
  let
    YearMonth { year: y1, month: m1 } = ym1
    YearMonth { year: y2, month: m2 } = ym2
  in
    (fromEnum y2 - fromEnum y1) * 12 + (fromEnum m2 - fromEnum m1) + 1
