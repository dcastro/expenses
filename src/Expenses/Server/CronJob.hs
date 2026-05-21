module Expenses.Server.CronJob where

import Config (AppConfig)
import Config qualified
import Control.Lens
import CustomPrelude
import Data.Aeson as J
import Data.Aeson.Encode.Pretty qualified as J
import Data.Aeson.Text qualified as J
import Data.Text qualified as T
import Data.Time (pattern YearMonthDay)
import Database qualified as Db
import Effectful
import Effectful.Exception qualified as Eff
import Effectful.FileSystem qualified as FS
import Effectful.FileSystem.IO.ByteString.Lazy qualified as FS
import Effectful.Reader.Static (asks)
import Effectful.Time qualified as Time
import Expenses.Effects
import Expenses.Effects qualified as Eff
import Expenses.Effects.EventLog qualified as EventLog
import Expenses.Effects.Nordigen qualified as N
import Expenses.Linear (liftConsume)
import Expenses.Server.EventLog qualified as EventLog
import Log
import System.Cron.Schedule (addJob, execSchedule)
import System.FilePath ((</>))
import Types
import Util qualified

startCronJobs :: Env -> Logger -> LogT IO ()
startCronJobs env logger = do
  let schedule = env.config.cronSchedule
  logInfo_ [i|Scheduling Nordigen sync job: #{schedule}.|]
  void $
    liftIO $ execSchedule do
      addJob (nordigenJob & Eff.runCronM env logger) schedule

nordigenJob ::
  forall es.
  (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es, Time :> es, EventLog :> es, Concurrent :> es, Db :> es) =>
  Eff es ()
nordigenJob =
  Eff.handleSync logCronFailure do
    nordigenJob'
 where
  logCronFailure :: SomeException -> Eff es ()
  logCronFailure err =
    logAttention_ [i|[Cron] Nordigen sync failed: #{displayException err}|]

nordigenJob' ::
  (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es, Time :> es, EventLog :> es, Concurrent :> es, Db :> es) =>
  Eff es ()
nordigenJob' = do
  logInfo_ "[Cron] Starting Nordigen sync job."
  now <- Time.currentTime
  txRows <- fetchAllAccounts now
  logInfo_ [i|[Cron] Fetched #{length txRows} transactions.|]
  useConnection \conn -> do
    newTxRows <- Db.filterNewTxs conn txRows
    for_ newTxRows \newTx -> do
      Db.insertTransactionJoinedRow conn newTx
      EventLog.appendEvent $ mkEventLogAction Config.cronUser now newTx
    logSyncTime now
    logInfo_ [i|[Cron] Nordigen sync succeeded. Transactions inserted: #{length newTxRows}.|]

fetchAllAccounts ::
  (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es) =>
  UTCTime -> Eff es [Db.TransactionJoinedRow]
fetchAllAccounts now = do
  config <- asks @Env (.config)
  join <$> forM config.accountInfos \acc ->
    do
      fetchAccount now acc
      -- NOTE: use the `*deep` version to catch any impure exceptions thrown by `getTransactionId`
      `Eff.catchSyncDeep` \err -> do
        -- Log the error, return no transactions, and move onto the next account
        logAttention_
          [i|
            [Cron] Failed to fetch account: #{acc ^. accountName}:
            #{displayException err}|]
        pure []

fetchAccount ::
  (Reader Env :> es, FileSystem :> es, Nordigen :> es, Log :> es) =>
  UTCTime -> AccountInfo -> Eff es [Db.TransactionJoinedRow]
fetchAccount now acc = do
  json <- N.getTransactions acc.accountId

  -- log response
  logTransactions now json acc

  -- decode json
  case fromJSON @TransactionResponse json of
    J.Success response -> do
      let apiTxs :: [ApiTransaction] =
            response
              ^.. transactions
                . booked
                . each
                . to fixTransaction
      config <- asks @Env (.config)
      pure $ apiToRow config acc <$> apiTxs
    J.Error err -> do
      logAttention_
        [i|
          [Cron] Failed to decode Nordigen's response for account: #{acc ^. accountName}:
            #{err}|]
      pure []

-- | Manually fix mistakes in the API response.
fixTransaction :: ApiTransaction -> ApiTransaction
fixTransaction tx =
  {-
    This transaction (found in the logs: `2026-05-17 02:00:00.080614225 UTC-transactions.json`)
    contained an invalid date: "5207-01-20".

    I backed up the db on `2026-05-19_16-10-31` before manually deleting this row and re-syncing.

    ```
    {
      "entryReference": "02026051552026-05-16-01.09.08.38441",
      "bookingDate": "5207-01-20",
      "valueDate": "5207-01-20",
      "transactionAmount": {
        "amount": "+0.04",
        "currency": "EUR"
      },
      "remittanceInformationUnstructured": "4PAG BXVAL- 8003 VIAVERDE",
      "internalTransactionId": "96aec454d5ec65f5ae62d8649e38afb6"
    },
    ```

    > delete from transactions where id = '02026051552026-05-16-01.09.08.38441';
    > delete from transaction_items where transaction_id = '02026051552026-05-16-01.09.08.38441';
  -}
  if tx.entryReference == Just "02026051552026-05-16-01.09.08.38441"
    then
      let date = YearMonthDay 2026 05 15
       in tx{bookingDate = date, valueDate = date}
    else tx

logTransactions :: (FileSystem :> es, Reader Env :> es) => UTCTime -> Value -> AccountInfo -> Eff es ()
logTransactions now transactions acc = do
  logsDir <- asks @Env (.logsDir)
  let accountDir = logsDir </> toString acc.accountName
  let archiveTransactionsDir = accountDir </> "archive-transactions"
  FS.createDirectoryIfMissing True archiveTransactionsDir

  -- Log the full JSON response to a file (pretty-printed), and also archive it with a timestamp (compact printed).
  FS.writeFile (accountDir </> "transactions.json") (J.encodePretty transactions)
  FS.writeFile (archiveTransactionsDir </> show now <> "-transactions.json") (J.encode transactions)

logSyncTime :: (FileSystem :> es, Reader Env :> es) => UTCTime -> Eff es ()
logSyncTime now = do
  logsDir <- asks @Env (.logsDir)
  let syncLogFile = logsDir </> "last-sync.md"
  FS.appendFile syncLogFile (show now <> "\n")

mkEventLogAction :: Admin %1 -> UTCTime %1 -> Db.TransactionJoinedRow %1 -> EventLog.Action
mkEventLogAction
  admin
  now
  Db.TransactionJoinedRow
    { transactionId
    , account
    , date
    , desc
    , totalAmountCents
    , isExpense
    , itemIndex
    , itemAmountCents
    , tag
    , details
    } = do
    -- Ignore `itemAmountCents`, it's always equal to `totalAmountCents`.
    liftConsume itemAmountCents do
      EventLog.Action
        { username = admin
        , ts = now
        , transactionId = transactionId
        , transactionDesc = desc
        , itemIndex = itemIndex
        , actionType =
            EventLog.NewTx
              EventLog.MkNewTx
                { account = account
                , date = date
                , totalAmountCents = totalAmountCents
                , isExpense = isExpense
                , tag = tag
                , details = details
                }
        }

apiToRow :: AppConfig -> AccountInfo -> ApiTransaction -> Db.TransactionJoinedRow
apiToRow config acc tx = do
  let txAmount = tx.transactionAmount.amount & fixSign acc & Util.eurosToCents & BECents
  let txId = getTransactionId tx
  let txDesc = tx.remittanceInformationUnstructured
  Db.TransactionJoinedRow
    { transactionId = txId
    , account = acc.accountName
    , date = tx.bookingDate
    , desc = txDesc
    , totalAmountCents = txAmount
    , isExpense = getIsExpense config acc txId txDesc
    , itemIndex = 0
    , itemAmountCents = txAmount
    , tag = pickCategory tx.remittanceInformationUnstructured
    , details = ""
    }
 where
  pickCategory :: Text -> Maybe TagName
  pickCategory desc =
    config.categoryPatterns ^? each . filtered (\entry -> entry.pattern_ `T.isInfixOf` desc) . to (.tag)

  fixSign :: AccountInfo -> Text -> Text
  fixSign ai amt =
    if ai.flipSign
      then
        if T.isPrefixOf "-" amt
          then T.drop 1 amt
          else "-" <> amt
      else amt

  getTransactionId :: ApiTransaction -> Text
  getTransactionId t =
    case t.transactionId <|> t.entryReference of
      Just tid -> tid
      Nothing ->
        error [i|Transaction does not have a 'transactionId' or a 'entryReference': '#{J.encodeToTextBuilder t}'|]

-- Determines whether a transaction should be considered an expense.
getIsExpense :: AppConfig -> AccountInfo -> Text -> Text -> Bool
getIsExpense config acc txId txDesc =
  if
    | not acc.isExpenseAccount -> False
    | not isExpenseTransaction -> False
    | hasTemporaryTxId -> False
    | otherwise -> True
 where
  isExpenseTransaction :: Bool
  isExpenseTransaction =
    flip all config.notExpenses \ptrn ->
      not (ptrn `T.isInfixOf` txDesc)

  {-
    Cetelem sometimes assigns temporary IDs to transactions, and then a few days later
    replaces them with other permanent IDs.
    These temporary IDs all seem to have 14 characters (including the surrounding quotes)
    Example: 0D087900031229
             33406237928299
    Example of a permanent ID: 20220121233851916940

    To double-check this, I made sure only transactions from Black and CaetanoGo had IDs with 14 characters.

    select * from transactions where length(id) = 14
    Returns 349 rows
    select * from transactions where length(id) = 14 and account <> 'Black' and account <> 'CaetanoGo'
    Returns 0 rows
  -}
  hasTemporaryTxId :: Bool
  hasTemporaryTxId =
    length txId == 14
