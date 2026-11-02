module Expenses.Server.GetReviewTransactionsSpec where

import CustomPrelude
import Data.Time (Day, UTCTime (..), fromGregorian)
import Database qualified as Db
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time qualified as Time
import Expenses.Server.Routes.GetReviewTransactions (getReviewTransactionsHandler)
import Expenses.Server.Routes.GetTransactions (TransactionItem (..))
import Expenses.Test.Util qualified as Util
import Test.Syd (Spec, describe, it, shouldBe)
import Types

spec :: Spec
spec = describe "getReviewTransactionsHandler" do
  it "returns items from the last 6 months without a tag or with a pending suggestion" do
    conn <- Util.mkInMemoryDbConn

    let
      recent = fromGregorian 2026 10 30
      rows =
        [ -- Included: no tag.
          mkRow "no-tag" recent True Nothing "" Nothing
        , -- Included: no tag, even though it's not an expense.
          mkRow "no-tag-not-expense" recent False Nothing "" Nothing
        , -- Included: pending suggestion.
          mkRow "pending-suggestion" recent True (Just "toys") "" (Just "JOYIN Kit de Slime para Festas")
        , -- Included: exactly 6 months ago.
          mkRow "six-months-ago" (fromGregorian 2026 5 2) True Nothing "" Nothing
        , -- Excluded: has a tag, and no suggestion.
          mkRow "tagged" recent True (Just "toys") "" Nothing
        , -- Excluded: has a tag and details, the suggestion is ignored.
          mkRow "tagged-with-details" recent True (Just "toys") "Slime" (Just "JOYIN Kit de Slime para Festas")
        , -- Excluded: older than 6 months.
          mkRow "too-old" (fromGregorian 2026 5 1) True Nothing "" Nothing
        ]

    forM_ rows \row ->
      SQL.useConnection (\c -> Db.insertTransactionJoinedRow c row)
        & SQL.runSQLiteSync conn
        & runConcurrent
        & runEff

    items <-
      getReviewTransactionsHandler
        & SQL.runSQLiteSync conn
        & Time.runFrozenTime (UTCTime (fromGregorian 2026 11 2) 0)
        & runConcurrent
        & runEff

    map (.transactionId) items
      `shouldBe` ["no-tag", "no-tag-not-expense", "pending-suggestion", "six-months-ago"]

mkRow :: Text -> Day -> Bool -> Maybe TagName -> Text -> Maybe Text -> Db.TransactionJoinedRow
mkRow txId date isExpense tag details detailsSuggestion =
  Db.TransactionJoinedRow
    { transactionId = txId
    , account = "bank"
    , date
    , desc = "desc"
    , totalAmountCents = BECents (-10_00)
    , isExpense
    , itemIndex = 0
    , itemAmountCents = BECents (-10_00)
    , tag
    , details
    , detailsSuggestion
    }
