module Expenses.Server.CronJob where

import Config qualified
import Control.Exception.Safe qualified as Safe
import Control.Lens
import CustomPrelude
import Data.Aeson as J
import Data.Aeson.Encode.Pretty qualified as J
import Data.Aeson.Text qualified as J
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database qualified as Db
import Expenses.Linear (liftConsume)
import Expenses.Server.AppM (Env (..), runLogger, useConnection)
import Expenses.Server.EventLog qualified as EventLog
import Log
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Nordigen qualified as N
import Servant.Auth.Client qualified as SA
import System.Cron.Schedule (addJob, execSchedule)
import System.Directory
import System.FilePath ((</>))
import Types
import Util qualified

type CronM = ReaderT Env (LogT IO)

startCronJobs :: (MonadLog m, MonadIO m) => Env -> Logger -> m ()
startCronJobs env logger = do
  logInfo_ [i|Scheduling Nordigen sync job: #{Config.cronSchedule}.|]
  void $
    liftIO $ execSchedule do
      addJob (nordigenJob env logger) Config.cronSchedule

nordigenJob :: Env -> Logger -> IO ()
nordigenJob env logger =
  Safe.handleAny (logCronFailure logger) do
    runCronM env logger do
      nordigenJob'
 where
  runCronM :: Env -> Logger -> CronM a -> IO a
  runCronM env logger action =
    action
      & flip runReaderT env
      & runLogger True logger

  logCronFailure :: Logger -> SomeException -> IO ()
  logCronFailure logger err =
    runLogger True logger do
      logAttention_ [i|[Cron] Nordigen sync failed: #{displayException err}|]

nordigenJob' :: CronM ()
nordigenJob' = do
  logInfo_ "[Cron] Starting Nordigen sync job."
  now <- liftIO getCurrentTime
  manager <- liftIO $ newManager tlsManagerSettings
  txRows <- fetchAllAccounts manager now Config.accountInfos
  logInfo_ [i|[Cron] Fetched #{length txRows} transactions.|]
  useConnection \conn -> do
    newTxRows <- liftIO $ Db.filterNewTxs conn txRows
    for_ newTxRows \newTx -> do
      Db.insertTransactionJoinedRow conn newTx
      EventLog.append $ mkEventLogAction Config.cronUser now newTx
    logSyncTime now
    logInfo_ [i|[Cron] Nordigen sync succeeded. Transactions inserted: #{length newTxRows}.|]

login :: Manager -> CronM SA.Token
login manager = do
  env <- ask
  ntr <- liftIO do
    N.runNordigen manager do
      N.getNewToken
        NewTokenRequest
          { secretId = env.nordigenSecretId
          , secretKey = env.nordigenSecretKey
          }
  pure $ SA.Token $ encodeUtf8 ntr.access

fetchAllAccounts :: Manager -> UTCTime -> [AccountInfo] -> CronM [Db.TransactionJoinedRow]
fetchAllAccounts manager now accs = do
  authToken <- login manager
  join <$> forM accs \acc ->
    do
      fetchAccount manager authToken now acc
      -- NOTE: use the `*deep` version to catch any impure exceptions thrown by `getTransactionId`
      `Safe.catchAnyDeep` \err -> do
        -- Log the error, return no transactions, and move onto the next account
        logAttention_
          [i|
            [Cron] Failed to fetch account: #{acc ^. accountName}:
            #{displayException err}|]
        pure []

fetchAccount :: Manager -> SA.Token -> UTCTime -> AccountInfo -> CronM [Db.TransactionJoinedRow]
fetchAccount manager authToken now acc = do
  json <- liftIO $ N.runNordigen manager do
    N.getTransactions authToken acc.accountId

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
      pure $ apiToRow acc <$> apiTxs
    J.Error err -> do
      logAttention_
        [i|
          [Cron] Failed to decode Nordigen's response for account: #{acc ^. accountName}:
            #{err}|]
      pure []

logTransactions :: UTCTime -> Value -> AccountInfo -> CronM ()
logTransactions now transactions acc = do
  logsDir <- asks (.logsDir)
  let accountDir = logsDir </> toString acc.accountName
  let archiveTransactionsDir = accountDir </> "archive-transactions"
  liftIO do
    createDirectoryIfMissing True archiveTransactionsDir

    -- Log the full JSON response to a file (pretty-printed), and also archive it with a timestamp (compact printed).
    BSL.writeFile (accountDir </> "transactions.json") (J.encodePretty transactions)
    J.encodeFile (archiveTransactionsDir </> show now <> "-transactions.json") transactions

logSyncTime :: UTCTime -> CronM ()
logSyncTime now = do
  logsDir <- asks (.logsDir)
  let syncLogFile = logsDir </> "last-sync.md"
  appendFile syncLogFile (show now <> "\n")

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

apiToRow :: AccountInfo -> ApiTransaction -> Db.TransactionJoinedRow
apiToRow acc tx = do
  let txAmount = tx.transactionAmount.amount & fixSign acc & Util.eurosToCents & BECents
  let txId = getTransactionId tx
  let txDesc = tx.remittanceInformationUnstructured
  Db.TransactionJoinedRow
    { transactionId = txId
    , account = acc.accountName
    , date = tx.bookingDate
    , desc = txDesc
    , totalAmountCents = txAmount
    , isExpense = getIsExpense acc txId txDesc
    , itemIndex = 0
    , itemAmountCents = txAmount
    , tag = pickCategory tx.remittanceInformationUnstructured
    , details = ""
    }
 where
  pickCategory :: Text -> Maybe TagName
  pickCategory desc =
    Config.categoryPatterns ^? each . filtered (\(ptrn, _) -> ptrn `T.isInfixOf` desc) . _2

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
getIsExpense :: AccountInfo -> Text -> Text -> Bool
getIsExpense acc txId txDesc =
  if
    | not acc.isExpenseAccount -> False
    | not isExpenseTransaction -> False
    | hasTemporaryTxId -> False
    | otherwise -> True
 where
  isExpenseTransaction :: Bool
  isExpenseTransaction =
    flip all Config.notExpenses \ptrn ->
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
