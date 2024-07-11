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
    ReqDagTime (..),
    ReqTaskInstance (..),
    mkFromTo,
    mkFrom,
    getTaskInstance,
    -- getTaskInstance',
    -- getTaskInstancesByStates,
    -- getTaskInstancesByStates',
    getFailedCeleryTask,
    getLastNTradeDaysRunId,
  )
where

import Contravariant.Extras (contrazip2, contrazip3, contrazip4)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime, UTCTime (UTCTime), defaultTimeLocale, parseTimeM)
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

data ReqDagTime = ReqDagTime
  { r_dag_id :: String,
    r_time :: ReqTime
  }

data ReqTaskInstance = ReqTaskInstance
  { r_dt :: ReqDagTime,
    r_state :: Maybe [TaskState]
  }

----------------------------------------------------------------------------------------------------
-- private DATs

data ReqTime
  = TimeRange
      { paramTime :: ParamTimeRange,
        timeFmt :: String
      }
  | LastNDays Int

data ParamTimeRange
  = FromTo String String
  | From String

data ParamTime' a
  = FromTo' a a
  | From' a

mkFromTo :: String -> (String, String) -> ReqTime
mkFromTo fmt (s, e) = TimeRange (FromTo s e) fmt

mkFrom :: String -> String -> ReqTime
mkFrom fmt s = TimeRange (From s) fmt

----------------------------------------------------------------------------------------------------
-- Export Fn
----------------------------------------------------------------------------------------------------

-- dag_id, cur_date, task_id -> TaskInstance
getTaskInstance :: Text -> Text -> Text -> R.Session (Maybe TaskInstance)
getTaskInstance dagId curDate taskId =
  TS.transaction TS.ReadCommitted TS.Read (tGetTaskInstance dagId curDate taskId)

-- dag_id, cur_date, states -> Vec<TaskInstance>
-- getTaskInstancesByStates :: Text -> Text -> [TaskState] -> R.Session (Vec.Vector TaskInstance)
-- getTaskInstancesByStates dagId curDate states =
--   TS.transaction TS.ReadCommitted TS.Read (tGetTaskInstancesByStates dagId curDate states)

getTaskInstance' :: ReqTaskInstance -> R.Session (Vec.Vector TaskInstance)
getTaskInstance' (ReqTaskInstance (ReqDagTime d t) st) =
  case t of
    tp@(TimeRange _ _) ->
      case reqTimeToParamT tp :: Maybe (ParamTime' UTCTime) of
        Just (FromTo' s e) -> undefined
        Just (From' s) -> undefined
        Nothing -> pure Vec.empty
    LastNDays n -> undefined

-- from_date, to_date -> Vec<CeleryTaskmeta>
-- from_date -> Vec<CeleryTaskmeta>
getFailedCeleryTask :: ReqTime -> R.Session (Vec.Vector CeleryTaskmeta)
getFailedCeleryTask timeParam = do
  let tp = reqTimeToParamT timeParam :: Maybe (ParamTime' LocalTime)
  case tp of
    Just (FromTo' s e) -> R.statement (s, e) sGetFailedCeleryTaskBtw
    Just (From' s) -> R.statement s sGetFailedCeleryTaskFrom
    Nothing -> pure Vec.empty

-- dag_id, pre_n_tradedays -> Vec<(run_id, cur_date)>
getLastNTradeDaysRunId :: Text -> Int -> R.Session (Vec.Vector (Text, Text))
getLastNTradeDaysRunId dagId prevNTradeDays =
  R.statement (dagId, fromIntegral prevNTradeDays) sGetLastNTradeDaysRunId

----------------------------------------------------------------------------------------------------
-- for debugging:

-- dag_id, run_id, task_id -> TaskInstance
-- getTaskInstance' :: Text -> Text -> Text -> R.Session (Maybe TaskInstance)
-- getTaskInstance' dagId runId taskId =
--   R.statement (dagId, runId, taskId) sGetTaskInstance

-- dag_id, run_id, states -> Vec<TaskInstance>
-- getTaskInstancesByStates' :: Text -> Text -> [TaskState] -> R.Session (Vec.Vector TaskInstance)
-- getTaskInstancesByStates' dagId runId states =
--   R.statement (dagId, runId, taskStateToStr <$> states) sGetTaskInstancesByStates

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

-- statement: dag_id, task_id -> Maybe<run_id>
sGetRunIdFromXCom :: S.Statement (Text, Text) (Maybe Text)
sGetRunIdFromXCom =
  let s =
        "select run_id from xcom \
        \where dag_id = $1 and task_id = 'branch_processor' and key = 'cur_date' \
        \and convert_from(value, 'UTF8') = '\"' || $2 || '\"'"
      e = contrazip2 eParamText eParamText
      d = D.rowMaybe $ D.column $ D.nonNullable D.text
   in S.Statement s e d True

-- statement: dag_id, run_id, task_id -> Maybe<TaskInstance>
sGetTaskInstance :: S.Statement (Text, Text, Text) (Maybe TaskInstance)
sGetTaskInstance =
  let s = sqlTaskInstance <> "where dag_id = $1 and run_id = $2 and task_id = $3"
      e = contrazip3 eParamText eParamText eParamText
      d = D.rowMaybe $ dRowTaskInstance
   in S.Statement s e d True

-- statement: dag_id, [run_id], [task_id] -> Vec<TaskInstance>
sGetTaskInstances :: S.Statement (Text, [Text], [Text]) (Vec.Vector TaskInstance)
sGetTaskInstances =
  let s = sqlTaskInstance <> "where dag_id = $1 and run_id = any ($2) and task_id = any ($3)"
      e = contrazip3 eParamText eParamListText eParamListText
      d = D.rowVector $ dRowTaskInstance
   in S.Statement s e d True

-- statement: dag_id, [run_id], [task_id], [state] -> Vec<TaskInstance>
sGetTaskInstances' :: S.Statement (Text, [Text], [Text], [Text]) (Vec.Vector TaskInstance)
sGetTaskInstances' =
  let s = sqlTaskInstance <> "where dag_id = $1 and run_id = any ($2) and task_id = any ($3) and state = any ($4)"
      e = contrazip4 eParamText eParamListText eParamListText eParamListText
      d = D.rowVector $ dRowTaskInstance
   in S.Statement s e d True

-- statement: from_date, to_date
sGetFailedCeleryTaskBtw :: S.Statement (LocalTime, LocalTime) (Vec.Vector CeleryTaskmeta)
sGetFailedCeleryTaskBtw =
  let s =
        "select task_id, date_done, \
        \(regexp_matches(traceback, '([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})'))[1] as ip_address \
        \from celery_taskmeta \
        \where status = 'FAILURE' and date_done between $1 and $2"
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

-- statement: dag_id, n
sGetLastNTradeDaysRunId :: S.Statement (Text, Int32) (Vec.Vector (Text, Text))
sGetLastNTradeDaysRunId =
  let s = sqlGetLastNTradeDaysRunId
      e = contrazip2 eParamText eParamI32
      d = D.rowVector $ (,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text)
   in S.Statement s e d True

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

-- TODO
sqlGetFromTradeDayRunId :: ByteString
sqlGetFromTradeDayRunId = undefined

-- TODO
sqlGetFromToTradeDayRunId :: ByteString
sqlGetFromToTradeDayRunId = undefined

sqlGetLastNTradeDaysRunId :: ByteString
sqlGetLastNTradeDaysRunId =
  "with previous_days as ( \
  \select generate_series( \
  \   current_date - interval '1 day' * ($2 - 1), \
  \   current_date, \
  \   interval '1 day' \
  \   )::date as date \
  \), \
  \run_ids as ( \
  \select run_id from xcom \
  \where \
  \   dag_id = $1 \
  \   and task_id = 'branch_processor' \
  \   and key = 'cur_date' \
  \   and convert_from(value, 'UTF8') = any ( \
  \     select '\"' || to_char(date, 'YYYYMMDD') || '\"' \
  \     from previous_days \
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
tGetTaskInstance :: Text -> Text -> Text -> T.Transaction (Maybe TaskInstance)
tGetTaskInstance dagId curDate taskId = do
  possibleRunId <- T.statement (dagId, curDate) sGetRunIdFromXCom
  case possibleRunId of
    Just runId -> T.statement (dagId, runId, taskId) sGetTaskInstance
    Nothing -> return Nothing

-- transaction: dag_id, cur_date, [state] -> T<Vec<TaskInstance>>
-- tGetTaskInstancesByStates :: Text -> Text -> [TaskState] -> T.Transaction (Vec.Vector TaskInstance)
-- tGetTaskInstancesByStates dagId curDate states = do
--   possibleRunId <- T.statement (dagId, curDate) sGetRunIdFromXCom
--   case possibleRunId of
--     Just runId -> T.statement (dagId, runId, taskStateToStr <$> states) sGetTaskInstancesByStates
--     Nothing -> return Vec.empty

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

-- encode param text
eParamText :: E.Params Text
eParamText = E.param $ E.nonNullable E.text

-- encode param [text]
eParamListText :: E.Params [Text]
eParamListText = E.param $ E.nonNullable $ E.foldableArray $ E.nonNullable E.text

eParamI32 :: E.Params Int32
eParamI32 = E.param $ E.nonNullable $ E.int4

-- encode param timestamp
eParamTimeStamp :: E.Params LocalTime
eParamTimeStamp = E.param $ E.nonNullable E.timestamp

-- work for XCom value
-- wrappedTextEncoder :: E.Params Text
-- wrappedTextEncoder = E.param $ E.nonNullable $ E.enum $ encodeWrappedText
--   where
--     encodeWrappedText text = "\"" <> text <> "\""

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

reqTimeToParamT :: (ToParamT a) => ReqTime -> Maybe (ParamTime' a)
reqTimeToParamT t =
  case paramTime t of
    FromTo s e -> FromTo' <$> toParamT f s <*> toParamT f e
    From s -> From' <$> toParamT f s
  where
    f = timeFmt t
