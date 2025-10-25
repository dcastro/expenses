module Database where

import Control.GroupWith qualified as GW
import CustomPrelude
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToField qualified as SQL
import Expenses.NonEmptyText (NonEmptyText)
import Log
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

filterNewTxs :: (MonadIO m) => Connection -> [TransactionJoinedRow] -> m [TransactionJoinedRow]
filterNewTxs conn txs = liftIO do
  let txIds = map (.transactionId) txs
  existingIds :: [Only Text] <-
    query
      conn
      ( "SELECT id FROM transactions WHERE id IN ("
          <> makePlaceholders (length txIds)
          <> ")"
      )
      txIds
  let existingSet = Set.fromList @Text (coerce existingIds)

  pure $ txs & filter (\tx -> not (tx.transactionId `Set.member` existingSet))

updateExistingRecord :: (MonadIO m) => Connection -> TransactionRecord -> m ()
updateExistingRecord conn txRecord =
  liftIO $ SQL.withTransaction conn do
    -- Delete the transaction's items and re-insert
    let (txRow, txItemRows) = recordToRows txRecord
    execute
      conn
      "DELETE FROM transaction_items WHERE transaction_id = ?"
      (Only txRow.transactionId)
    executeMany
      conn
      [sql|
        INSERT INTO transaction_items
          (transaction_id, item_index, item_amount_cents, tag, details, is_expense)
        VALUES
          (?, ?, ?, ?, ?, ?)
      |]
      txItemRows

getTransactionById :: Connection -> Text -> IO (Maybe TransactionRecord)
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

getTransactionItemById :: (MonadIO m) => Connection -> Text -> Int -> m (Maybe TransactionJoinedRow)
getTransactionItemById conn txId itemIndex = liftIO do
  SQL.query
    conn
    ( SQL.Query
        [i| #{selectJoinedRows} WHERE id = ? AND item_index = ?|]
    )
    (txId, itemIndex)
    <&> safeHead

getTransactionsByDate :: Connection -> Day -> Day -> IO [TransactionJoinedRow]
getTransactionsByDate conn startDate endDate = do
  SQL.query
    conn
    ( SQL.Query
        [i| #{selectJoinedRows} WHERE date >= ? AND date < ?|]
    )
    (startDate, endDate)

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
  | SomeTag TagName
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
  , value :: Maybe SQLData
  }
  deriving stock (Show, Eq)

mkClause :: forall a. (ToField a) => Text -> a -> WhereClause
mkClause sql value = WhereClause sql (Just $ SQL.toField value)

mkClauseWithoutVal :: Text -> WhereClause
mkClauseWithoutVal sql = WhereClause sql Nothing

search :: (MonadIO m, MonadLog m) => Connection -> SearchParams -> m (Vector TransactionJoinedRow)
search conn params = do
  let (query, values) = mkSearchQuery params
  txs <- Util.timed "search query" do
    liftIO $ fromList <$> SQL.query conn query values

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
            , clauses & mapMaybe (.value)
            )
  let query =
        SQL.Query $
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
    <> maybeToList do
      params.tag <&> \case
        NoTag -> mkIsNull tagCol
        SomeTag tag -> mkIsEq tagCol tag.unTagName.getNonEmptyText
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
-- Tags
----------------------------------------------------------------------------

getAllTags :: Connection -> IO [TagName]
getAllTags conn = do
  coerce $
    SQL.query_ @(Only TagName)
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

getAllAccounts :: Connection -> IO [Text]
getAllAccounts conn = do
  coerce $
    SQL.query_ @(Only Text)
      conn
      [sql|
        SELECT DISTINCT(account)
        FROM transactions
        ORDER BY account
      |]

----------------------------------------------------------------------------
-- Modify transactions
----------------------------------------------------------------------------

getDescription :: (MonadIO m) => Connection -> Text -> m Text
getDescription conn txId = liftIO do
  SQL.query conn [sql| SELECT desc FROM transactions WHERE id = ? |] [txId] >>= \case
    [Only desc] -> pure desc
    [] -> die $ T.unpack [i|getDescription: transaction not found for #{txId}|]
    _ -> die $ T.unpack [i|getDescription: unexpected number of rows for #{txId}|]

getTag :: (MonadIO m) => Connection -> Text -> Int -> m (Maybe TagName)
getTag conn txId idx = liftIO do
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
    [] -> die $ T.unpack [i|getTag: transaction item not found for #{txId} (#{idx})|]
    _ -> die $ T.unpack [i|getTag: unexpected number of rows for #{txId} (#{idx})|]

updateTag :: forall m. (MonadIO m) => Connection -> Text -> Int -> TagName -> m ()
updateTag conn txId idx newTag = liftIO do
  SQL.execute
    conn
    [sql|
        UPDATE transaction_items
        SET tag = ?
        WHERE transaction_id = ? AND item_index = ?
      |]
    (newTag, txId, idx)

getIsExpense :: (MonadIO m) => Connection -> Text -> Int -> m Bool
getIsExpense conn txId idx = liftIO do
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
    [] -> die $ T.unpack [i|getIsExpense: transaction not found for #{txId}|]
    _ -> die $ T.unpack [i|getIsExpense: unexpected number of rows for #{txId}|]

updateIsExpense :: (MonadIO m) => Connection -> Text -> Int -> Bool -> m ()
updateIsExpense conn txId idx newIsExpense = liftIO do
  SQL.execute
    conn
    [sql|
        UPDATE transaction_items
        SET is_expense = ?
        WHERE transaction_id = ? AND item_index = ?
      |]
    (newIsExpense, txId, idx)

getDetails :: (MonadIO m) => Connection -> Text -> Int -> m Text
getDetails conn txId idx = liftIO do
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
    [] -> die $ T.unpack [i|getDetails: transaction item not found for #{txId} (#{idx})|]
    _ -> die $ T.unpack [i|getDetails: unexpected number of rows for #{txId} (#{idx})|]

updateDetails :: forall m. (MonadIO m) => Connection -> Text -> Int -> Text -> m ()
updateDetails conn txId idx newDetails = liftIO do
  SQL.execute
    conn
    [sql|
        UPDATE transaction_items
        SET details = ?
        WHERE transaction_id = ? AND item_index = ?
      |]
    (newDetails, txId, idx)

insertTransactionJoinedRow :: forall m. (MonadIO m) => Connection -> TransactionJoinedRow -> m ()
insertTransactionJoinedRow conn TransactionJoinedRow{transactionId, account, date, desc, totalAmountCents, isExpense, itemIndex, itemAmountCents, tag, details} = liftIO do
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

-- >>> makePlaceholders 0
-- >>> makePlaceholders 1
-- >>> makePlaceholders 3
-- ""
-- "?"
-- "?, ?, ?"
makePlaceholders :: Int -> Query
makePlaceholders n =
  mconcat $ List.intersperse ", " (replicate n "?")

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

$( mconcat
     [ -- Used for deserializing /search requests
       deriveFromJSON defaultOptions ''TagParams
     ]
 )
