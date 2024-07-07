{-# LANGUAGE OverloadedStrings #-}

-- file: AfTasks.hs
-- author: Jacob Xie
-- date: 2024/07/07 18:36:16 Sunday
-- brief:

module OpsLib.AfTasks
  ( TaskInstance (..),
    XCom (..),
    getTaskInstance,
  )
where

import Contravariant.Extras (contrazip2)
import Contravariant.Extras.Contrazip (contrazip3)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector, fromList, toList)
import qualified Data.Vector as V
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
    state :: Maybe Text,
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

data XCom = XCom
  { x_dag_run_id :: Int32,
    x_task_id :: Text,
    x_key :: Text,
    x_dag_id :: Text,
    x_run_id :: Text,
    x_value :: Maybe ByteString,
    x_timestamp :: UTCTime
  }

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

-- statement: dag_id, task_id -> S<run_id>
sGetRunIdFromXCom :: S.Statement (Text, Text) (Maybe Text)
sGetRunIdFromXCom =
  let s =
        "select run_id from xcom \
        \where dag_id = $1 and task_id = 'branch_processor' and key = 'cur_date' and convert_from(value, 'UTF8') = $2"
      e = contrazip2 eParamText eParamText
      d = D.rowMaybe $ D.column $ D.nonNullable D.text
   in S.Statement s e d True

-- statement: dag_id, run_id, task_id -> S<TaskInstance>
sGetTaskInstance :: S.Statement (Text, Text, Text) (Maybe TaskInstance)
sGetTaskInstance =
  let s =
        "select dag_id, run_id, task_id, start_date, end_date, duration, state, try_number, max_tries, hostname, unixname, job_id, pool, pool_slots, priority_weight, operator, queued_dttm, queued_by_job_id, pid, updated_at, external_executor_id \
        \from task_instance where dag_id = $1 and run_id = $2 and task_id = $3"
      e = contrazip3 eParamText eParamText eParamText
      d =
        D.rowMaybe $
          TaskInstance
            <$> D.column (D.nonNullable D.text)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nullable D.timestamptz)
            <*> D.column (D.nullable D.timestamptz)
            <*> D.column (D.nullable D.float8)
            <*> D.column (D.nullable D.text)
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
   in S.Statement s e d True

-- encode param text
eParamText :: E.Params Text
eParamText = E.param $ E.nonNullable E.text

----------------------------------------------------------------------------------------------------

-- transaction: dag_id, cur_date, task_id -> T<TaskInstance>
tGetTaskInstance :: Text -> Text -> Text -> T.Transaction (Maybe TaskInstance)
tGetTaskInstance dagId curDate taskId = do
  possibleRunId <- T.statement (dagId, curDate) sGetRunIdFromXCom
  case possibleRunId of
    Just runId -> T.statement (dagId, runId, taskId) sGetTaskInstance
    Nothing -> return Nothing

----------------------------------------------------------------------------------------------------
-- Fn
----------------------------------------------------------------------------------------------------

-- dag_id, cur_date, task_id -> TaskInstance
getTaskInstance :: Text -> Text -> Text -> R.Session (Maybe TaskInstance)
getTaskInstance dagId curDate taskId =
  TS.transaction TS.ReadCommitted TS.Read (tGetTaskInstance dagId curDate taskId)

-- TODO
-- getTaskInstancesByState
