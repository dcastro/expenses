module Database where

import Control.GroupWith qualified as GW
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Coerce (coerce)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.Month (Month, pattern MonthDay)
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToField qualified as SQL
import Effectful
import Effectful.Log
import Effectful.SQLite.Simple
import Effectful.SQLite.Simple qualified as SQL
import Effectful.Time (Time)
import Expenses.Effects qualified as Eff
import Expenses.NonEmptyText (NonEmptyText)
import Types
import Util qualified

-- | Represents the joining of the tables `transactions` and `transaction_items`.
data TransactionJoinedRow = TransactionJoinedRow
  { transactionId :: Text
  , account :: Text
  , date :: Day
  , desc :: Text
  , totalAmountCents :: BECents
  , isExpense :: Bool
  , itemIndex :: Int
  , itemAmountCents :: BECents
  , tag :: Maybe TagName
  , details :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

instance FromRow TransactionJoinedRow where
  fromRow =
    TransactionJoinedRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

selectJoinedRows :: Text
selectJoinedRows =
  [i|
    SELECT id, account, date, desc, total_amount_cents, is_expense, item_index, item_amount_cents, tag, details
    FROM transactions t JOIN transaction_items ti ON t.id = ti.transaction_id
  |]

-- | A row from the table `transactions`.
data TransactionRow = TransactionRow
  { transactionId :: Text
  , account :: Text
  , date :: Day
  , desc :: Text
  , totalAmountCents :: BECents
  }
  deriving stock (Show)

instance FromRow TransactionRow where
  fromRow =
    TransactionRow <$> field <*> field <*> field <*> field <*> field

instance ToRow TransactionRow where
  toRow TransactionRow{transactionId, account, date, desc, totalAmountCents} =
    toRow (transactionId, account, date, desc, totalAmountCents)

-- | A row from the table `transaction_items`.
data TransactionItemRow = TransactionItemRow
  { transactionId :: Text
  , itemIndex :: Int
  , itemAmountCents :: BECents
  , tag :: Maybe TagName
  , details :: Text
  , isExpense :: Bool
  }
  deriving stock (Show)

instance FromRow TransactionItemRow where
  fromRow =
    TransactionItemRow <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TransactionItemRow where
  toRow TransactionItemRow{transactionId, itemIndex, itemAmountCents, tag, details, isExpense} =
    toRow (transactionId, itemIndex, itemAmountCents, tag, details, isExpense)

data SyncAccountStatusRow = SyncAccountStatusRow
  { accountId :: Text
  , accountName :: Text
  , lastSyncFinishedAt :: UTCTime
  , lastSyncStatus :: SyncStatus
  , lastSyncError :: Maybe Text
  , lastSyncedTransactionCount :: Int
  , updatedAt :: UTCTime
  }
  deriving stock (Show, Eq)

instance FromRow SyncAccountStatusRow where
  fromRow =
    SyncAccountStatusRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

filterNewTxs :: (SQLite :> es) => Connection -> [TransactionJoinedRow] -> Eff es [TransactionJoinedRow]
filterNewTxs conn txs = do
  let txIds = map (.transactionId) txs
  existingIds :: [Only Text] <-
    SQL.query
      conn
      ( Query
          [i|
          SELECT id FROM transactions WHERE id IN (#{mkPlaceholders (length txIds)})
        |]
      )
      txIds
  let existingSet = Set.fromList @Text (coerce existingIds)

  pure $ txs & filter (\tx -> not (tx.transactionId `Set.member` existingSet))

updateExistingRecord :: (SQLite :> es) => Connection -> TransactionRecord -> Eff es ()
updateExistingRecord conn txRecord =
  SQL.withTransaction conn do
    -- Delete the transaction's items and re-insert
    let (txRow, txItemRows) = recordToRows txRecord
    SQL.execute
      conn
      "DELETE FROM transaction_items WHERE transaction_id = ?"
      (Only txRow.transactionId)
    SQL.executeMany
      conn
      [sql|
        INSERT INTO transaction_items
          (transaction_id, item_index, item_amount_cents, tag, details, is_expense)
        VALUES
          (?, ?, ?, ?, ?, ?)
      |]
      txItemRows

getTransactionById :: (SQLite :> es) => Connection -> Text -> Eff es (Maybe TransactionRecord)
getTransactionById conn transactionId =
  runMaybeT do
    txRow <-
      SQL.query
        conn
        "SELECT id, account, date, desc, total_amount_cents FROM transactions WHERE id = ?"
        (Only transactionId)
        <&> safeHead
        & MaybeT

    itemRows <-
      lift $
        SQL.query
          conn
          "SELECT transaction_id, item_index, item_amount_cents, tag, details, is_expense FROM transaction_items WHERE transaction_id = ?"
          (Only transactionId)

    pure $ rowsToRecord txRow itemRows

getTransactionItemById :: (SQLite :> es) => Connection -> Text -> Int -> Eff es (Maybe TransactionJoinedRow)
getTransactionItemById conn txId itemIndex = do
  SQL.query
    conn
    ( Query
        [i| #{selectJoinedRows} WHERE id = ? AND item_index = ?|]
    )
    (txId, itemIndex)
    <&> safeHead

getTransactionsByDate :: (SQLite :> es) => Connection -> Day -> Day -> Eff es [TransactionJoinedRow]
getTransactionsByDate conn startDate endDate = do
  SQL.query
    conn
    ( Query
        [i| #{selectJoinedRows} WHERE date >= ? AND date < ?|]
    )
    (startDate, endDate)

getTransactionsMonthRange :: (SQLite :> es) => Connection -> Eff es (Maybe (Month, Month))
getTransactionsMonthRange conn = do
  rows :: [(Maybe Day, Maybe Day)] <-
    SQL.query_
      conn
      [sql|
        SELECT MIN(date), MAX(date)
        FROM transactions
      |]
  pure $ case rows of
    [(Just minDay, Just maxDay)] -> Just (dayToMonth minDay, dayToMonth maxDay)
    _ -> Nothing
 where
  dayToMonth :: Day -> Month
  dayToMonth (MonthDay month _) = month

recordToRows :: TransactionRecord -> (TransactionRow, [TransactionItemRow])
recordToRows tx =
  ( TransactionRow
      { transactionId = tx.transactionId
      , account = tx.account
      , date = tx.date
      , desc = tx.desc
      , totalAmountCents = tx.totalAmountCents
      }
  , zipWith
      ( \idx item ->
          TransactionItemRow
            { transactionId = tx.transactionId
            , itemIndex = idx
            , itemAmountCents = item.itemAmountCents
            , tag = item.tag
            , details = item.details
            , isExpense = item.isExpense
            }
      )
      [0 ..]
      tx.items
  )

rowsToRecord :: TransactionRow -> [TransactionItemRow] -> TransactionRecord
rowsToRecord txRow itemRows =
  TransactionRecord
    { transactionId = txRow.transactionId
    , account = txRow.account
    , date = txRow.date
    , desc = txRow.desc
    , totalAmountCents = txRow.totalAmountCents
    , items =
        map
          ( \tir ->
              TransactionItemRecord
                { itemAmountCents = tir.itemAmountCents
                , tag = tir.tag
                , details = tir.details
                , isExpense = tir.isExpense
                }
          )
          (sortOn (.itemIndex) itemRows)
    }

-- | Groups transaction items into transaction records.
rowsToRecords :: [TransactionRow] -> [TransactionItemRow] -> [TransactionRecord]
rowsToRecords txRows itemRows = do
  let itemsByTxId = GW.groupWith (.transactionId) itemRows
  txRows <&> \txRow ->
    rowsToRecord txRow (Map.findWithDefault [] txRow.transactionId itemsByTxId)

----------------------------------------------------------------------------
-- Search
----------------------------------------------------------------------------

data SearchParams = SearchParams
  { allFields :: StringParams
  , transactionId :: Maybe Text
  , date :: Maybe Contains
  , account :: Maybe Text
  , desc :: StringParams
  , amount :: Maybe AmountParams
  , tag :: Maybe TagParams
  , notes :: StringParams
  , isExpense :: Maybe Bool
  }
  deriving stock (Eq)

data StringParams = StringParams
  { contains :: [Contains]
  , doesNotContain :: [DoesNotContain]
  }
  deriving stock (Show, Eq)

data TagParams
  = NoTag
  | SomeTags [TagName]
  deriving stock (Eq)

data AmountParams
  = AmountContains Contains
  | AmountIsGTE IsGTE
  | AmountIsLT IsLT
  deriving stock (Show, Eq)

newtype Contains = Contains {getContains :: NonEmptyText}
  deriving newtype (Show, Eq, ToField)

newtype DoesNotContain = DoesNotContain {getDoesNotContain :: NonEmptyText}
  deriving newtype (Show, Eq, ToField)

newtype IsGTE = IsGTE {isGTE :: Double}
  deriving newtype (Show, Eq, ToField)

newtype IsLT = IsLT {isLT :: Double}
  deriving newtype (Show, Eq, ToField)

data WhereClause = WhereClause
  { sql :: Text
  , values :: [SQLData]
  }
  deriving stock (Show, Eq)

mkClause :: forall a. (ToField a) => Text -> a -> WhereClause
mkClause sql value = WhereClause sql [SQL.toField value]

mkClauseWithoutVal :: Text -> WhereClause
mkClauseWithoutVal sql = WhereClause sql []

search :: (SQLite :> es, Log :> es, Time :> es) => Connection -> SearchParams -> Eff es (Vector TransactionJoinedRow)
search conn params = do
  let (query, values) = mkSearchQuery params
  txs <- Util.timed "search query" do
    fromList <$> SQL.query conn query values

  logTrace_ [i|Found #{length txs} matching transaction items.|]
  pure txs

mkSearchQuery :: SearchParams -> (Query, [SQLData])
mkSearchQuery params = do
  let clauses = mkSearchQueryClauses params
  let (clausesStr, values) =
        if null clauses
          then ("TRUE", [])
          else
            ( T.intercalate " AND \n\t" ((.sql) <$> clauses)
            , clauses >>= (.values)
            )
  let query =
        Query $
          [i|
    #{selectJoinedRows}
    WHERE
        #{clausesStr}|]
  (query, values)

mkSearchQueryClauses :: SearchParams -> [WhereClause]
mkSearchQueryClauses params =
  mkStringParams descCol params.desc
    <> maybeToList do
      date <- params.date
      Just $ mkContains dateCol date
    <> maybeToList do
      txId <- params.transactionId
      Just $ mkIsEq txIdCol txId
    <> maybeToList do
      account <- params.account
      Just $ mkIsEq accountCol account
    <> case params.tag of
      Nothing -> []
      Just NoTag -> [mkIsNull tagCol]
      Just (SomeTags []) -> []
      Just (SomeTags tags) ->
        [ WhereClause
            [i|#{tagCol} IN (#{mkPlaceholders (length tags)})|]
            (SQL.toField . (.unTagName.getNonEmptyText) <$> tags)
        ]
    <> mkStringParams detailsCol params.notes
    <> maybeToList do
      params.amount <&> \case
        AmountContains contains -> mkContains itemAmountTextCol contains
        AmountIsGTE isGTE -> mkIsGTE itemAmountNumericCol isGTE
        AmountIsLT isLT -> mkIsLT itemAmountNumericCol isLT
    <> maybeToList do
      isExpense <- params.isExpense
      Just $ mkIsEq isExpenseCol (if isExpense then "1" else "0")
    <>
    -- Concatenate all columns and search for the string in the result string.
    mkStringParams fullTextSearchCols params.allFields
 where
  txIdCol = "id"
  descCol = "desc"
  accountCol = "account"
  dateCol = "strftime('%d-%m-%Y', date)"
  tagCol = "tag"
  detailsCol = "details"
  itemAmountNumericCol = "CAST(-item_amount_cents as REAL) / 100"
  itemAmountTextCol = [i|printf('%.2f', #{itemAmountNumericCol})|]
  isExpenseCol = "is_expense"
  fullTextSearchCols =
    [i|(#{coalesce dateCol} || ' ' || #{coalesce descCol} || ' ' || #{coalesce tagCol} || ' ' || #{coalesce detailsCol} || ' ' || #{coalesce itemAmountTextCol})|]

  coalesce :: Text -> Text
  coalesce a = [i|COALESCE(#{a}, '')|]

  mkStringParams :: Text -> StringParams -> [WhereClause]
  mkStringParams field (StringParams contains doesNotContain) =
    let containsClauses = mkContains field <$> contains
        doesNotContainClauses = mkDoesNotContain field <$> doesNotContain
     in (containsClauses <> doesNotContainClauses)

  mkIsNull :: Text -> WhereClause
  mkIsNull field = mkClauseWithoutVal [i|#{field} IS NULL|]

  mkIsEq :: Text -> Text -> WhereClause
  mkIsEq field value = mkClause [i|#{field} = ?|] value

  mkIsGTE :: Text -> IsGTE -> WhereClause
  mkIsGTE field value = mkClause [i|#{field} >= ?|] value

  mkIsLT :: Text -> IsLT -> WhereClause
  mkIsLT field value = mkClause [i|#{field} < ?|] value

  mkContains :: Text -> Contains -> WhereClause
  mkContains field value = mkClause [i|LOWER(#{field}) GLOB ?|] (mkGlobPattern value.getContains.getNonEmptyText)

  mkDoesNotContain :: Text -> DoesNotContain -> WhereClause
  mkDoesNotContain field value = mkClause [i|LOWER(#{field}) NOT GLOB ?|] (mkGlobPattern value.getDoesNotContain.getNonEmptyText)

-- >>> mkGlobPattern "ÁgUa"
-- "*[a\225\224\226\227]g[u\250\249][a\225\224\226\227]*"
mkGlobPattern :: Text -> Text
mkGlobPattern str =
  -- See: https://stackoverflow.com/a/55724736/857807
  str
    & T.toLower
    & replaceEquivChars
    & \pat -> [i|*#{pat}*|]

-- >>> replaceEquivChars "água"
-- "[a\225\224\226\227]g[u\250\249][a\225\224\226\227]"
replaceEquivChars :: Text -> Text
replaceEquivChars =
  T.concatMap \c ->
    case Map.lookup c equivalentChars of
      Nothing -> T.singleton c
      Just equivSet -> [i|[#{equivSet}]|]
 where
  equivalentChars :: Map Char String
  equivalentChars = Map.fromList do
    equivSet <- groups
    c <- equivSet
    pure (c, equivSet)

  -- Support for equivalent diacritics.
  groups :: [[Char]]
  groups = ["aáàâã", "eéèê", "iíì", "oóòôõ", "uúù", "cç"]

----------------------------------------------------------------------------
-- App settings
----------------------------------------------------------------------------

getBudgetLimit :: (SQLite :> es) => Connection -> Eff es BECents
getBudgetLimit conn = do
  rows <- SQL.query_ conn [sql| SELECT monthly_budget_limit_cents FROM app_settings WHERE id = 1 |]
  case rows of
    [Only limit] -> pure limit
    _ -> error "getBudgetLimit: app_settings must have exactly 1 row"

setBudgetLimit :: (SQLite :> es) => Connection -> BECents -> Eff es ()
setBudgetLimit conn limitCents =
  SQL.execute
    conn
    [sql| UPDATE app_settings SET monthly_budget_limit_cents = ? WHERE id = 1 |]
    (Only limitCents)

----------------------------------------------------------------------------
-- Tags
----------------------------------------------------------------------------

getAllTags :: (SQLite :> es) => Connection -> Eff es [TagName]
getAllTags conn = do
  coerce @(_ _ [Only TagName]) @(_ _ [TagName]) $
    SQL.query_ @_ @(Only TagName)
      conn
      [sql|
        SELECT DISTINCT(tag)
        FROM transaction_items
        WHERE tag IS NOT NULL
        ORDER BY tag
      |]

----------------------------------------------------------------------------
-- Accounts
----------------------------------------------------------------------------

getAllAccounts :: (SQLite :> es) => Connection -> Eff es [Text]
getAllAccounts conn = do
  coerce $
    SQL.query_ @_ @(Only Text)
      conn
      [sql|
        SELECT DISTINCT(account)
        FROM transactions
        ORDER BY account
      |]

----------------------------------------------------------------------------
-- Sync account status
----------------------------------------------------------------------------

upsertSyncAccountStatus ::
  (SQLite :> es) =>
  Connection ->
  Text ->
  Text ->
  UTCTime ->
  SyncStatus ->
  Maybe Text ->
  Int ->
  Eff es ()
upsertSyncAccountStatus conn accountId accountName finishedAt status err txCount = do
  SQL.execute
    conn
    [sql|
      INSERT INTO sync_account_status
        (account_id, account_name, last_sync_finished_at, last_sync_status, last_sync_error, last_synced_transaction_count)
      VALUES
        (?, ?, ?, ?, ?, ?)
      ON CONFLICT(account_id) DO UPDATE SET
        account_name = excluded.account_name,
        last_sync_finished_at = excluded.last_sync_finished_at,
        last_sync_status = excluded.last_sync_status,
        last_sync_error = excluded.last_sync_error,
        last_synced_transaction_count = excluded.last_synced_transaction_count,
        updated_at = CURRENT_TIMESTAMP
    |]
    (accountId, accountName, finishedAt, status, err, txCount)

getSyncAccountStatuses :: (SQLite :> es) => Connection -> Eff es [SyncAccountStatusRow]
getSyncAccountStatuses conn =
  SQL.query_
    conn
    [sql|
      SELECT
        account_id,
        account_name,
        last_sync_finished_at,
        last_sync_status,
        last_sync_error,
        last_synced_transaction_count,
        updated_at
      FROM sync_account_status
      ORDER BY account_name ASC
    |]

----------------------------------------------------------------------------
-- Modify transactions
----------------------------------------------------------------------------

getDescription :: (SQLite :> es) => Connection -> Text -> Eff es Text
getDescription conn txId = do
  SQL.query conn [sql| SELECT desc FROM transactions WHERE id = ? |] [txId] >>= \case
    [Only desc] -> pure desc
    [] -> Eff.die [i|getDescription: transaction not found for #{txId}|]
    _ -> Eff.die [i|getDescription: unexpected number of rows for #{txId}|]

getTag :: (SQLite :> es) => Connection -> Text -> Int -> Eff es (Maybe TagName)
getTag conn txId idx = do
  rows <-
    SQL.query
      conn
      [sql|
          SELECT tag
          FROM transaction_items
          WHERE transaction_id = ? AND item_index = ?
        |]
      (txId, idx)
  case rows of
    [Only tag] -> pure tag
    [] -> Eff.die [i|getTag: transaction item not found for #{txId} (#{idx})|]
    _ -> Eff.die [i|getTag: unexpected number of rows for #{txId} (#{idx})|]

updateTag :: (SQLite :> es) => Connection -> Text -> Int -> TagName -> Eff es ()
updateTag conn txId idx newTag = do
  SQL.execute
    conn
    [sql|
        UPDATE transaction_items
        SET tag = ?
        WHERE transaction_id = ? AND item_index = ?
      |]
    (newTag, txId, idx)

getIsExpense :: (SQLite :> es) => Connection -> Text -> Int -> Eff es Bool
getIsExpense conn txId idx = do
  rows <-
    SQL.query
      conn
      [sql|
          SELECT is_expense
          FROM transaction_items
          WHERE transaction_id = ? AND item_index = ?
        |]
      (txId, idx)
  case rows of
    [Only flag] -> pure flag
    [] -> Eff.die [i|getIsExpense: transaction not found for #{txId}|]
    _ -> Eff.die [i|getIsExpense: unexpected number of rows for #{txId}|]

updateIsExpense :: (SQLite :> es) => Connection -> Text -> Int -> Bool -> Eff es ()
updateIsExpense conn txId idx newIsExpense = do
  SQL.execute
    conn
    [sql|
        UPDATE transaction_items
        SET is_expense = ?
        WHERE transaction_id = ? AND item_index = ?
      |]
    (newIsExpense, txId, idx)

getDetails :: (SQLite :> es) => Connection -> Text -> Int -> Eff es Text
getDetails conn txId idx = do
  rows <-
    SQL.query
      conn
      [sql|
          SELECT details
          FROM transaction_items
          WHERE transaction_id = ? AND item_index = ?
        |]
      (txId, idx)
  case rows of
    [Only details] -> pure details
    [] -> Eff.die [i|getDetails: transaction item not found for #{txId} (#{idx})|]
    _ -> Eff.die [i|getDetails: unexpected number of rows for #{txId} (#{idx})|]

updateDetails :: (SQLite :> es) => Connection -> Text -> Int -> Text -> Eff es ()
updateDetails conn txId idx newDetails = do
  SQL.execute
    conn
    [sql|
        UPDATE transaction_items
        SET details = ?
        WHERE transaction_id = ? AND item_index = ?
      |]
    (newDetails, txId, idx)

insertTransactionJoinedRow :: (SQLite :> es) => Connection -> TransactionJoinedRow -> Eff es ()
insertTransactionJoinedRow conn TransactionJoinedRow{transactionId, account, date, desc, totalAmountCents, isExpense, itemIndex, itemAmountCents, tag, details} = do
  SQL.withTransaction conn do
    SQL.execute
      conn
      [sql|
          INSERT INTO transactions (id, account, date, desc, total_amount_cents)
          VALUES (?, ?, ?, ?, ?)
        |]
      (transactionId, account, date, desc, totalAmountCents)
    SQL.execute
      conn
      [sql|
          INSERT INTO transaction_items (transaction_id, item_index, item_amount_cents, tag, details, is_expense)
          VALUES (?, ?, ?, ?, ?, ?)
        |]
      (transactionId, itemIndex, itemAmountCents, tag, details, isExpense)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- >>> mkPlaceholders 0
-- >>> mkPlaceholders 1
-- >>> mkPlaceholders 3
-- ""
-- "?"
-- "?, ?, ?"
mkPlaceholders :: Int -> Text
mkPlaceholders n = T.intercalate ", " (replicate n "?")

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

$( mconcat
     [ -- Used for deserializing /search requests
       deriveFromJSON defaultOptions ''TagParams
     ]
 )
