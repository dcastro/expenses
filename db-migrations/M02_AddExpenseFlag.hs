module M02_AddExpenseFlag where

import CustomPrelude
import Data.Text qualified as T
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)

{- |

This migration:
* adds an `is_expense` boolean flag to the `transactions` db

Ran on 06/06/2025.

Migration task list:
* Disable the anacron job
* Backup gogle sheet sheet
* Backup database
* Advance "start from" in the config to the current month
* stack run expenses:exe:db-migrations -- 2
* Enable the anacron job
-}
migrate :: Connection -> IO ()
migrate conn = do
  SQL.execute_
    conn
    [sql| ALTER TABLE transactions ADD COLUMN is_expense BOOLEAN; |]

  SQL.fold_
    conn
    [sql| SELECT id, account, desc FROM transactions |]
    ()
    \() (id :: Text, account :: Text, desc :: Text) -> do
      let isExpense =
            isExpenseAccount account
              && not (shouldBeIgnored desc)
              && not (hasTemporaryTxId id)
      SQL.execute
        conn
        [sql|
          UPDATE transactions SET is_expense = ? WHERE id = ?
        |]
        (isExpense, id)
 where
  -- Determines whether a transaction should be added to the spreadsheet or ignored.
  -- If it should be ignored, it is not an expense, and therefore should not be added to the expenses spreadsheet.
  shouldBeIgnored :: Text -> Bool
  shouldBeIgnored desc =
    flip any notExpenses \ptrn ->
      ptrn `T.isInfixOf` desc

  isExpenseAccount :: Text -> Bool
  isExpenseAccount = \case
    "Activo" -> False
    _ -> True

  {-
    Cetelem sometimes assigns temporary IDs to transactions, and then a few days later
    replaces them with other permanent IDs.
    These temporary IDs all seem to have 16 characters.
    Example: "0D087900031229"
             "33406237928299"
    Example of a permanent ID: "20220121233851916940"

    To double-check this, I made sure only transactions from Black and CaetanoGo had IDs with 16 characters.

    select * from transactions where length(id) = 16
    Returns 349 rows
    select * from transactions where length(id) = 16 and account <> 'Black' and account <> 'CaetanoGo'
    Returns 0 rows
  -}

  hasTemporaryTxId :: Text -> Bool
  hasTemporaryTxId tid =
    length tid == 16

-- | Transactions with this substring will not be treated as expenses.
notExpenses :: [Text]
notExpenses =
  [ "TRF DE DIOGO FILIPE AZEVEDO CASTRO"
  , "TRF P  Diogo Moey" -- Millennium -> Moey
  , "TRANSF SEPA -ENG DIOGO FILIPE" -- Millennium -> Moey
  , "DD PT41100946 CETELEM         75526515837" -- black
  , "DD PT41100946 CETELEM         75498308722" -- caetano go
  , "DD PT24113086 UNIVERSO"
  , "DD UNIVERSO"
  , -- Black
    "Pagamento de Mensalidade"
  , -- Salário da Sónia
    "TRF. P O  FLOATINGLICIOUS - LDA       MADEMOISE"
  , -- Nao vou ignorar, porque posso querer adicionar algo manualmente no campo "Details"
    -- , "DEPOSITO NUMERARIO"

    -- Transferência da conta Millennium da Sónia
    "TRF DE ENF SONIA DANIELA CARNEIRO BARBOSA"
  , -- Levantamento ATM multibanco
    "LEV ATM "
  , "REEMBOLSOS   IRS"
  ]
