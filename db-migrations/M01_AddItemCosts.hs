module M01_AddItemCosts where

import CustomPrelude hiding (i)
import Data.String.Interpolate (i)
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.Types (Only (..))
import Util qualified

{- |

This migration:
* add item costs to transactions.
* changes the `amount` column from a REAL to an INTEGER, and values are now stored as cents of Euro.

Ran on 05/06/2025.

Migration task list:
* Disable the anacron job
* Backup gogle sheet sheet
* Backup database
* Advance "start from" in the config to the current month
* Google sheet:
  * Add a new column "Item Costs" to the latest transactions sheet.
  * Copy over the values from the "Total Costs" column to the "Item Costs" column.
  * Add a new column "Item Costs" to the template sheet.
  * Change the "Refund" column to be calculated using the "Item Costs" column instead of the "Total Costs" column.
  * Change the formula in Cell B2 to use "item costs" instead of "total costs".
  * Change the array formula in Cell A3 to use "item costs" instead of "total costs".

* Ensure FKs are enabled: `PRAGMA foreign_keys = ON;`
* stack run expenses:exe:db-migrations -- 1
* Run `select * from sqlite_master;` and check the final schema matches the expected.
* Enable the anacron job

NOTES:
  The migration initially failed because I had two rows in the database where the amount was "text" and not a number.
    select * from transactions where amount LIKE '%€'
    id	account	date	desc	amount	tag	details	created_on	baby
    "EXP-f6a03a03-fccc-418c-938c-6075012b0479"	Universo	2022-02-23	Compra AMAZON.ES*2R8MX7P64 AM Luxembourg	-26.83€	-	gym bottle	2022-03-06 11:21:30
    "EXP-18c3c3b0-9cd2-4d91-b615-885b0879e873"	Universo	2022-02-09	Compra A.S.RIO TINTO RIO TINTO	-41.46€	gasolina		2022-03-06 11:21:30

select count(*) from transactions;
4812
-}
migrate :: Connection -> IO ()
migrate conn = do
  replaceAmountWithTotalAmountCents conn
  addTransactionItemsTable conn

replaceAmountWithTotalAmountCents :: Connection -> IO ()
replaceAmountWithTotalAmountCents conn = do
  SQL.execute_
    conn
    [sql| ALTER TABLE transactions ADD COLUMN total_amount_cents INTEGER; |]
  -- NOTE: my initial thought was to run this:
  --   UPDATE transactions SET total_amount_cents = trunc(amount * 100);
  -- But sqlite has rounding errors, here's an example:
  --   select trunc(-33.98 * 100) == -3397.0
  -- Which means we have to update the rows in Haskell land.
  SQL.fold_
    conn
    [sql| SELECT id, CAST(amount as TEXT) FROM transactions |]
    ()
    \() (id :: Text, amount :: Text) ->
      SQL.execute
        conn
        [sql|
          UPDATE transactions SET total_amount_cents = ? WHERE id = ?
        |]
        (Util.eurosToCents amount, id)

  -- SANITY CHECKS
  [Only @Int mismatchRowCount] <-
    SQL.query_
      conn
      [sql|
          SELECT COUNT(*)
          FROM transactions
          WHERE CAST(total_amount_cents as REAL) / 100 <> amount;
      |]
  when (mismatchRowCount /= 0) do
    die [i|ERROR: Mismatch in total_amount_cents and amount! Mismatch row count: #{mismatchRowCount}|]

  [(sumAmount :: Float, sumTotalAmountCents :: Float)] <-
    SQL.query_
      conn
      [sql|
          SELECT SUM(amount), CAST(SUM(total_amount_cents) as REAL) / 100
          FROM transactions
      |]
  when (sumAmount /= sumTotalAmountCents) do
    die [i|ERROR: sumAmount #{sumAmount} /= #{sumTotalAmountCents} sumTotalAmountCents|]

  -- FINISH
  SQL.execute_
    conn
    [sql| ALTER TABLE transactions DROP COLUMN amount; |]

addTransactionItemsTable :: Connection -> IO ()
addTransactionItemsTable conn = do
  SQL.execute_
    conn
    [sql|
      CREATE TABLE transaction_items (
        transaction_id TEXT NOT NULL REFERENCES transactions(id),
        item_index INTEGER NOT NULL,

        item_amount_cents INTEGER NOT NULL,
        tag TEXT,
        details TEXT,
        baby BOOLEAN,

        created_on DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,

        UNIQUE(transaction_id, item_index)
      )
    |]
  rows <-
    SQL.query_
      conn
      [sql|
        SELECT id, total_amount_cents, tag, details, baby FROM transactions
      |]

  forM_ rows \(transaction_id :: Text, totalAmountCents :: Int, tag :: Text, details :: Text, baby :: Maybe Bool) -> do
    SQL.execute
      conn
      [sql|
        INSERT INTO transaction_items
          (transaction_id, item_index, item_amount_cents, tag, details, baby)
        VALUES
          (?, ?, ?, ?, ?, ?)
      |]
      (transaction_id, 0 :: Int, totalAmountCents, tag, details, baby)

  -- SANITY CHECKS
  [Only @Int transactionsRowCount] <- SQL.query_ conn [sql|SELECT COUNT(*) FROM transactions|]
  [Only @Int transactionItemsRowCount] <- SQL.query_ conn [sql|SELECT COUNT(*) FROM transaction_items|]
  when (transactionsRowCount /= transactionItemsRowCount) do
    die [i|ERROR: transactionsRowCount #{transactionsRowCount} /= #{transactionItemsRowCount} transactionItemsRowCount|]

  [Only @Int transactionsWithMismatch] <-
    SQL.query_
      conn
      [sql|
        SELECT COUNT(*)
        FROM transactions t LEFT JOIN transaction_items ti ON t.id = ti.transaction_id
        WHERE
          t.total_amount_cents <> ti.item_amount_cents OR
          t.tag                <> ti.tag OR
          t.details            <> ti.details OR
          t.baby               <> ti.baby;
      |]
  when (transactionsWithMismatch /= 0) do
    die [i|ERROR: transactionsWithMismatch #{transactionsWithMismatch} /= 0|]

  -- FINISH
  SQL.execute_ conn [sql| ALTER TABLE transactions DROP COLUMN tag; |]
  SQL.execute_ conn [sql| ALTER TABLE transactions DROP COLUMN details; |]
  SQL.execute_ conn [sql| ALTER TABLE transactions DROP COLUMN baby; |]
