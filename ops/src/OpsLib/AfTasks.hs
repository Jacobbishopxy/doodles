{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: AfTasks.hs
-- author: Jacob Xie
-- date: 2024/07/07 18:36:16 Sunday
-- brief:

module OpsLib.AfTasks
  ( TaskInstance (..),
    TaskState (..),
    CeleryTaskmeta (..),
    ReqTime,
    ReqTaskInstance,
    mkReqFromTo,
    mkReqFrom,
    mkReqIs,
    mkReqLastN,
    mkReqTaskInstance,
    getTaskInstance,
    getFailedCeleryTask,
    getLastNTradeDaysRunId,
  )
where

import Contravariant.Extras (contrazip2, contrazip3, contrazip4)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Time (LocalTime, UTCTime, defaultTimeLocale, parseTimeM)
import qualified Data.Vector as Vec
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as R
import qualified Hasql.Statement as S
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as TS

----------------------------------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------------------------------

data TaskInstance = TaskInstance
  { dag_id :: Text,
    run_id :: Text,
    task_id :: Text,
    start_date :: Maybe UTCTime,
    end_date :: Maybe UTCTime,
    duration :: Maybe Double,
    state :: Maybe TaskState,
    try_number :: Maybe Int32,
    max_tries :: Maybe Int32,
    hostname :: Maybe Text,
    unixname :: Maybe Text,
    job_id :: Maybe Int32,
    pool :: Text,
    pool_slots :: Int32,
    priority_weight :: Maybe Int32,
    operator :: Maybe Text,
    queued_dttm :: Maybe UTCTime,
    queued_by_job_id :: Maybe Int32,
    pid :: Maybe Int32,
    updated_at :: Maybe UTCTime,
    external_executor_id :: Maybe Text
  }
  deriving (Show)

-- Task state
data TaskState
  = TsDeferred
  | TsFailed
  | TsQueued
  | TsRemoved
  | TsRestarting
  | TsRunning
  | TsScheduled
  | TsShutdown
  | TsSkipped
  | TsSuccess
  | TsUpForReschedule
  | TsUpForRetry
  | TsUpstreamFailed
  | TsNone
  deriving (Show)

data CeleryTaskmeta = CeleryTaskmeta
  { c_task_id :: Maybe Text,
    c_date_done :: Maybe LocalTime,
    c_ip :: Maybe Text
  }
  deriving (Show)

type ParamTimeFmt = String

data ReqTime
  = TimeRange ParamTimeRange ParamTimeFmt
  | LastNDays Int

data ReqTaskInstance = ReqTaskInstance String ReqTime (Maybe [String]) (Maybe [TaskState])

mkReqFromTo :: String -> (String, String) -> ReqTime
mkReqFromTo fmt (s, e) = TimeRange (FromTo s e) fmt

mkReqFrom :: String -> String -> ReqTime
mkReqFrom fmt s = TimeRange (From s) fmt

mkReqIs :: String -> String -> ReqTime
mkReqIs fmt s = TimeRange (Is s) fmt

mkReqLastN :: Int -> ReqTime
mkReqLastN n = LastNDays n

mkReqTaskInstance :: String -> ReqTime -> Maybe [String] -> Maybe [TaskState] -> ReqTaskInstance
mkReqTaskInstance dagId reqTime taskIds states = ReqTaskInstance dagId reqTime taskIds states

----------------------------------------------------------------------------------------------------
-- private DATs

data ParamTimeRange
  = FromTo String String
  | From String
  | Is String

data ParamTimeRange' a
  = FromTo' a a
  | From' a
  | Is' a

----------------------------------------------------------------------------------------------------
-- Export Fn
----------------------------------------------------------------------------------------------------

-- ReqTaskInstance -> Vec<TaskInstance>
getTaskInstance :: ReqTaskInstance -> R.Session (Vec.Vector TaskInstance)
getTaskInstance =
  TS.transaction TS.ReadCommitted TS.Read . tGetTaskInstances

-- ReqTime -> Vec<CeleryTaskmeta>
getFailedCeleryTask :: ReqTime -> R.Session (Vec.Vector CeleryTaskmeta)
getFailedCeleryTask t = do
  case t of
    LastNDays n -> R.statement (fromIntegral n) sGetFailedCeleryTaskPrevN
    tp -> case reqTimeToParamT tp :: Maybe (ParamTimeRange' LocalTime) of
      Just (FromTo' s e) -> R.statement (s, e) sGetFailedCeleryTaskBtw
      Just (From' s) -> R.statement s sGetFailedCeleryTaskFrom
      Just (Is' d) -> R.statement d sGetFailedCeleryTaskIn
      Nothing -> pure Vec.empty

-- dag_id, pre_n_tradedays -> Vec<(run_id, cur_date)>
getLastNTradeDaysRunId :: Text -> Int -> R.Session (Vec.Vector (Text, Text))
getLastNTradeDaysRunId dagId prevNTradeDays =
  R.statement (dagId, fromIntegral prevNTradeDays) sGetLastNTradeDayRunIds

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- XCom

-- statement: dag_id, task_id -> Maybe<run_id>
-- sGetRunIdFromXCom :: S.Statement (Text, Text) (Maybe Text)
-- sGetRunIdFromXCom =
--   let s =
--         "select run_id from xcom \
--         \where dag_id = $1 and task_id = 'branch_processor' and key = 'cur_date' \
--         \and convert_from(value, 'UTF8') = '\"' || $2 || '\"'"
--       e = contrazip2 eParamText eParamText
--       d = D.rowMaybe $ D.column $ D.nonNullable D.text
--    in S.Statement s e d True

----------------------------------------------------------------------------------------------------
-- FailedCeleryTask

-- statement: from_date, to_date
sGetFailedCeleryTaskBtw :: S.Statement (LocalTime, LocalTime) (Vec.Vector CeleryTaskmeta)
sGetFailedCeleryTaskBtw =
  let s = sqlFailedCeleryTask <> "and date_done between $1 and $2"
      e = contrazip2 eParamTimeStamp eParamTimeStamp
      d = D.rowVector $ dRowCeleryTaskmeta
   in S.Statement s e d True

-- statement: from_date
sGetFailedCeleryTaskFrom :: S.Statement LocalTime (Vec.Vector CeleryTaskmeta)
sGetFailedCeleryTaskFrom =
  let s = sqlFailedCeleryTask <> "and date_done >= $1"
      e = eParamTimeStamp
      d = D.rowVector $ dRowCeleryTaskmeta
   in S.Statement s e d True

-- statement: date
sGetFailedCeleryTaskIn :: S.Statement LocalTime (Vec.Vector CeleryTaskmeta)
sGetFailedCeleryTaskIn =
  let s = sqlFailedCeleryTask <> "and date_done between $1 and $1 + interval '1 day'"
      e = eParamTimeStamp
      d = D.rowVector $ dRowCeleryTaskmeta
   in S.Statement s e d True

-- statement: n
sGetFailedCeleryTaskPrevN :: S.Statement Int32 (Vec.Vector CeleryTaskmeta)
sGetFailedCeleryTaskPrevN =
  let s =
        sqlWithPrevNDays
          <> sqlFailedCeleryTask
          <> "and date_done >= (select date_before_n from date_before_n)"
      e = eParamI32
      d = D.rowVector $ dRowCeleryTaskmeta
   in S.Statement s e d True

----------------------------------------------------------------------------------------------------
-- TaskInstance

-- statement: dag_id, run_id, task_id -> Maybe<TaskInstance>
-- sGetTaskInstance :: S.Statement (Text, Text, Text) (Maybe TaskInstance)
-- sGetTaskInstance =
--   let s = sqlTaskInstance <> "where dag_id = $1 and run_id = $2 and task_id = $3"
--       e = contrazip3 eParamText eParamText eParamText
--       d = D.rowMaybe dRowTaskInstance
--    in S.Statement s e d True

-- statement: dag_id, [run_id], Maybe [task_id], Maybe [TaskState]
mkSGetTaskInstances :: (Maybe [Text], Maybe [Text]) -> S.Statement (Text, [Text], Maybe [Text], Maybe [Text]) (Vec.Vector TaskInstance)
mkSGetTaskInstances (taskIds, states) = do
  let sqlBase = sqlTaskInstance <> "where dag_id = $1 and run_id = any ($2) "
      cond1 = if isNothing taskIds then "" else "and task_id = any ($3) "
      cond2 = if isNothing states then "" else "and state = any ($4) "
      finalSql = sqlBase <> cond1 <> cond2
      e = contrazip4 eParamText eParamListText eParamMListText eParamMListText
      d = D.rowVector dRowTaskInstance
   in S.Statement finalSql e d True

----------------------------------------------------------------------------------------------------
-- TradeDayRunIds

-- statement: dag_id, from_date
sGetFromTradeDayRunIds :: S.Statement (Text, UTCTime) (Vec.Vector (Text, Text))
sGetFromTradeDayRunIds =
  let s = sqlGetFromTradeDayRunIds
      e = contrazip2 eParamText eParamTimeStamptz
      d = D.rowVector dRowRunIds
   in S.Statement s e d True

-- statement: dag_id, from_date, to_date
sGetFromToTradeDayRunIds :: S.Statement (Text, UTCTime, UTCTime) (Vec.Vector (Text, Text))
sGetFromToTradeDayRunIds =
  let s = sqlGetFromToTradeDayRunIds
      e = contrazip3 eParamText eParamTimeStamptz eParamTimeStamptz
      d = D.rowVector dRowRunIds
   in S.Statement s e d True

-- statement: dag_id, date
sGetIsTradeDayRunIds :: S.Statement (Text, UTCTime) (Vec.Vector (Text, Text))
sGetIsTradeDayRunIds =
  let s = sqlGetIsTradeDayRunIds
      e = contrazip2 eParamText eParamTimeStamptz
      d = D.rowVector dRowRunIds
   in S.Statement s e d True

-- statement: dag_id, n
sGetLastNTradeDayRunIds :: S.Statement (Text, Int32) (Vec.Vector (Text, Text))
sGetLastNTradeDayRunIds =
  let s = sqlGetLastNTradeDaysRunIds
      e = contrazip2 eParamText eParamI32
      d = D.rowVector dRowRunIds
   in S.Statement s e d True

-- statement: ReqDagTime

----------------------------------------------------------------------------------------------------

sqlTaskInstance :: ByteString
sqlTaskInstance =
  "select dag_id, run_id, task_id, start_date, end_date, duration, state, try_number, max_tries, \
  \hostname, unixname, job_id, pool, pool_slots, priority_weight, operator, queued_dttm, \
  \queued_by_job_id, pid, updated_at, external_executor_id from task_instance "

sqlFailedCeleryTask :: ByteString
sqlFailedCeleryTask =
  "select task_id, date_done, \
  \(regexp_matches(traceback, '([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})'))[1] as ip_address \
  \from celery_taskmeta \
  \where status = 'FAILURE' "

----------------------------------------------------------------------------------------------------

-- N days
sqlWithPrevNDays :: ByteString
sqlWithPrevNDays =
  "with date_before_n as ( \
  \   select (current_date - interval '1 day' * $1)::date as date_before_n\
  \) "

-- From
sqlGetFromTradeDayRunIds :: ByteString
sqlGetFromTradeDayRunIds =
  "with dates as ( \
  \SELECT generate_series( \
  \   $2, \
  \   current_date, \
  \   interval '1 day' \
  \   )::date AS date \
  \), "
    <> sqlSelectRunIds

-- From, To
sqlGetFromToTradeDayRunIds :: ByteString
sqlGetFromToTradeDayRunIds =
  "with dates as ( \
  \SELECT generate_series( \
  \   $2, \
  \   $3, \
  \   interval '1 day' \
  \   )::date AS date \
  \), "
    <> sqlSelectRunIds

-- Date
sqlGetIsTradeDayRunIds :: ByteString
sqlGetIsTradeDayRunIds =
  "with dates as ( \
  \SELECT generate_series( \
  \   $2, \
  \   $2, \
  \   interval '1 day' \
  \   )::date AS date \
  \), "
    <> sqlSelectRunIds

-- N
sqlGetLastNTradeDaysRunIds :: ByteString
sqlGetLastNTradeDaysRunIds =
  "with dates as ( \
  \select generate_series( \
  \   current_date - interval '1 day' * ($2 - 1), \
  \   current_date, \
  \   interval '1 day' \
  \   )::date as date \
  \), "
    <> sqlSelectRunIds

-- from 'dates' to get run_id which is trading date
sqlSelectRunIds :: ByteString
sqlSelectRunIds =
  "run_ids as ( \
  \select run_id from xcom \
  \where \
  \   dag_id = $1 \
  \   and task_id = 'branch_processor' \
  \   and key = 'cur_date' \
  \   and convert_from(value, 'UTF8') = any ( \
  \     select '\"' || to_char(date, 'YYYYMMDD') || '\"' \
  \     from dates \
  \   ) \
  \) \
  \select run_id, value \
  \   from xcom \
  \   where \
  \   dag_id = $1 \
  \   and task_id = 'branch_processor' \
  \   and key = 'cur_date' \
  \   and run_id in (select run_id from run_ids)"

----------------------------------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------------------------------

-- transaction: dag_id, cur_date, task_id -> T<TaskInstance>
-- tGetTaskInstance :: Text -> Text -> Text -> T.Transaction (Maybe TaskInstance)
-- tGetTaskInstance dagId curDate taskId = do
--   possibleRunId <- T.statement (dagId, curDate) sGetRunIdFromXCom
--   case possibleRunId of
--     Just runId -> T.statement (dagId, runId, taskId) sGetTaskInstance
--     Nothing -> return Nothing

-- transaction: ReqTaskInstance -> T<Vec<TaskInstance>>
tGetTaskInstances :: ReqTaskInstance -> T.Transaction (Vec.Vector TaskInstance)
tGetTaskInstances (ReqTaskInstance dagId reqTime taskIds taskStates) = do
  runIds <- case reqTime of
    LastNDays n -> T.statement (di, fromIntegral n) sGetLastNTradeDayRunIds
    tp -> case reqTimeToParamT tp of
      Just (FromTo' s e) -> T.statement (di, s, e) sGetFromToTradeDayRunIds
      Just (From' s) -> T.statement (di, s) sGetFromTradeDayRunIds
      Just (Is' d) -> T.statement (di, d) sGetIsTradeDayRunIds
      Nothing -> pure Vec.empty

  T.statement (di, ri runIds, ti, ts) $ mkSGetTaskInstances (ti, ts)
  where
    ri runIds' = Vec.toList $ fst <$> runIds'
    di = pack dagId
    ti = (pack <$>) <$> taskIds
    ts = (taskStateToStr <$>) <$> taskStates

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

-- encode param text
eParamText :: E.Params Text
eParamText = E.param $ E.nonNullable E.text

-- encode param [text]
eParamListText :: E.Params [Text]
eParamListText = E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable E.text

eParamMListText :: E.Params (Maybe [Text])
eParamMListText = E.param $ E.nullable $ E.foldableArray $ E.nonNullable E.text

eParamI32 :: E.Params Int32
eParamI32 = E.param $ E.nonNullable $ E.int4

-- encode param timestamp
eParamTimeStamp :: E.Params LocalTime
eParamTimeStamp = E.param $ E.nonNullable E.timestamp

eParamTimeStamptz :: E.Params UTCTime
eParamTimeStamptz = E.param $ E.nonNullable E.timestamptz

-- work for XCom value
-- wrappedTextEncoder :: E.Params Text
-- wrappedTextEncoder = E.param $ E.nonNullable $ E.enum $ encodeWrappedText
--   where
--     encodeWrappedText text = "\"" <> text <> "\""

----------------------------------------------------------------------------------------------------

-- decode TaskInstance
dRowTaskInstance :: D.Row TaskInstance
dRowTaskInstance =
  TaskInstance
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nullable D.timestamptz)
    <*> D.column (D.nullable D.timestamptz)
    <*> D.column (D.nullable D.float8)
    <*> D.column (D.nullable decodeTaskState)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.text)
    <*> D.column (D.nullable D.text)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.int4)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.text)
    <*> D.column (D.nullable D.timestamptz)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.int4)
    <*> D.column (D.nullable D.timestamptz)
    <*> D.column (D.nullable D.text)

-- decode TaskState
decodeTaskState :: D.Value TaskState
decodeTaskState = D.enum $ \case
  "deferred" -> Just TsDeferred
  "failed" -> Just TsFailed
  "queued" -> Just TsQueued
  "removed" -> Just TsRemoved
  "restarting" -> Just TsRestarting
  "running" -> Just TsRunning
  "scheduled" -> Just TsScheduled
  "shutdown" -> Just TsShutdown
  "skipped" -> Just TsSkipped
  "success" -> Just TsSuccess
  "up_for_reschedule" -> Just TsUpForReschedule
  "up_for_retry" -> Just TsUpForRetry
  "upstream_failed" -> Just TsUpstreamFailed
  _ -> Nothing

-- decode CeleryTaskmeta
dRowCeleryTaskmeta :: D.Row CeleryTaskmeta
dRowCeleryTaskmeta =
  CeleryTaskmeta
    <$> D.column (D.nullable D.text)
    <*> D.column (D.nullable D.timestamp)
    <*> D.column (D.nullable D.text)

-- decode TaskInstance rows
dRowRunIds :: D.Row (Text, Text)
dRowRunIds = (,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text)

----------------------------------------------------------------------------------------------------

taskStateToStr :: TaskState -> Text
taskStateToStr s =
  case s of
    TsDeferred -> "deferred"
    TsFailed -> "failed"
    TsQueued -> "queued"
    TsRemoved -> "removed"
    TsRestarting -> "restarting"
    TsRunning -> "running"
    TsScheduled -> "scheduled"
    TsShutdown -> "shutdown"
    TsSkipped -> "skipped"
    TsSuccess -> "success"
    TsUpForReschedule -> "up_for_reschedule"
    TsUpForRetry -> "up_for_retry"
    TsUpstreamFailed -> "upstream_failed"
    TsNone -> "null"

----------------------------------------------------------------------------------------------------

class ToParamT a where
  -- fmt, time_string -> Maybe a
  toParamT :: String -> String -> Maybe a

instance ToParamT LocalTime where
  toParamT = parseTimeM True defaultTimeLocale

instance ToParamT UTCTime where
  toParamT = parseTimeM True defaultTimeLocale

reqTimeToParamT :: (ToParamT a) => ReqTime -> Maybe (ParamTimeRange' a)
reqTimeToParamT (TimeRange t f) = case t of
  FromTo s e -> FromTo' <$> toParamT f s <*> toParamT f e
  From s -> From' <$> toParamT f s
  Is d -> Is' <$> toParamT f d
reqTimeToParamT _ = error "unsupported `ReqTime`"
