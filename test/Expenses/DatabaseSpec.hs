module Expenses.DatabaseSpec where

import CustomPrelude
import Database (TagParams (..), WhereClause, mkClause, mkClauseWithoutVal, mkSearchQueryClauses)
import Expenses.Server.Routes.Search (RawSearchParams (..), parseSearchParams)
import Expenses.Test.Util ()
import Test.Hspec (Spec, it)
import Test.Hspec.Expectations.Pretty (Expectation, shouldBe)

emptyParams :: RawSearchParams
emptyParams =
  RawSearchParams
    { allFields = ""
    , date = ""
    , account = Nothing
    , desc = ""
    , amount = ""
    , tag = Nothing
    , notes = ""
    , isExpense = Nothing
    }

spec_mkSearchQueryClauses :: Spec
spec_mkSearchQueryClauses = do
  it "handles contains and doesNotContain for allFields only" $
    emptyParams
      { allFields = "Foó báAR -báZ"
      }
      `shouldQuery` [ mkClause @Text
                        "LOWER((COALESCE(strftime('%d-%m-%Y', date), '') || ' ' || COALESCE(desc, '') || ' ' || COALESCE(tag, '') || ' ' || COALESCE(details, '') || ' ' || COALESCE(printf('%.2f', CAST(-item_amount_cents as REAL) / 100), ''))) GLOB ?"
                        -- "%foo%"
                        "*f[oóòôõ][oóòôõ]*"
                    , mkClause @Text
                        "LOWER((COALESCE(strftime('%d-%m-%Y', date), '') || ' ' || COALESCE(desc, '') || ' ' || COALESCE(tag, '') || ' ' || COALESCE(details, '') || ' ' || COALESCE(printf('%.2f', CAST(-item_amount_cents as REAL) / 100), ''))) GLOB ?"
                        "*b[aáàâã][aáàâã]r*"
                    , mkClause @Text
                        "LOWER((COALESCE(strftime('%d-%m-%Y', date), '') || ' ' || COALESCE(desc, '') || ' ' || COALESCE(tag, '') || ' ' || COALESCE(details, '') || ' ' || COALESCE(printf('%.2f', CAST(-item_amount_cents as REAL) / 100), ''))) NOT GLOB ?"
                        "*b[aáàâã]z*"
                    ]

  it "handles all fields with values" $ do
    RawSearchParams
      { allFields = "fóõ -bAR id:\"abc\""
      , date = "2022-01-01"
      , account = Just "account1"
      , desc = "dêsc -no-desc"
      , amount = "100"
      , tag = Just $ SomeTag "tag1"
      , notes = "  nOTe  -not-NOTé  "
      , isExpense = Just True
      }
      `shouldQuery` [ mkClause @Text
                        "LOWER(desc) GLOB ?"
                        "*d[eéèê]s[cç]*"
                    , mkClause @Text
                        "LOWER(desc) NOT GLOB ?"
                        "*n[oóòôõ]-d[eéèê]s[cç]*"
                    , mkClause @Text
                        "LOWER(strftime('%d-%m-%Y', date)) GLOB ?"
                        "*2022-01-01*"
                    , mkClause @Text
                        "id = ?"
                        "\"abc\""
                    , mkClause @Text
                        "account = ?"
                        "account1"
                    , mkClause @Text
                        "tag = ?"
                        "tag1"
                    , mkClause @Text
                        "LOWER(details) GLOB ?"
                        "*n[oóòôõ]t[eéèê]*"
                    , mkClause @Text
                        "LOWER(details) NOT GLOB ?"
                        "*n[oóòôõ]t-n[oóòôõ]t[eéèê]*"
                    , mkClause @Text
                        "LOWER(printf('%.2f', CAST(-item_amount_cents as REAL) / 100)) GLOB ?"
                        "*100*"
                    , mkClause @Text
                        "is_expense = ?"
                        "1"
                    , mkClause @Text
                        "LOWER((COALESCE(strftime('%d-%m-%Y', date), '') || ' ' || COALESCE(desc, '') || ' ' || COALESCE(tag, '') || ' ' || COALESCE(details, '') || ' ' || COALESCE(printf('%.2f', CAST(-item_amount_cents as REAL) / 100), ''))) GLOB ?"
                        "*f[oóòôõ][oóòôõ]*"
                    , mkClause @Text
                        "LOWER((COALESCE(strftime('%d-%m-%Y', date), '') || ' ' || COALESCE(desc, '') || ' ' || COALESCE(tag, '') || ' ' || COALESCE(details, '') || ' ' || COALESCE(printf('%.2f', CAST(-item_amount_cents as REAL) / 100), ''))) NOT GLOB ?"
                        "*b[aáàâã]r*"
                    ]

  it "handles searching for txs with no tags" $ do
    emptyParams{tag = Just NoTag}
      `shouldQuery` [ mkClauseWithoutVal
                        "tag IS NULL"
                    ]

  it "handles amount filters" $ do
    emptyParams{amount = "100"}
      `shouldQuery` [ mkClause @Text
                        "LOWER(printf('%.2f', CAST(-item_amount_cents as REAL) / 100)) GLOB ?"
                        "*100*"
                    ]
    emptyParams{amount = ">100"}
      `shouldQuery` [ mkClause @Double
                        "CAST(-item_amount_cents as REAL) / 100 >= ?"
                        100
                    ]
    emptyParams{amount = " < 100.3 "}
      `shouldQuery` [ mkClause @Double
                        "CAST(-item_amount_cents as REAL) / 100 < ?"
                        100.3
                    ]

  it "handles empty params" $ do
    emptyParams `shouldQuery` []

shouldQuery :: RawSearchParams -> [WhereClause] -> Expectation
shouldQuery params expected = mkSearchQueryClauses (parseSearchParams params) `shouldBe` expected
