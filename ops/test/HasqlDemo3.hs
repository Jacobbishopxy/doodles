{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- file: HasqlDemo3.hs
-- author: Jacob Xie
-- date: 2024/07/06 11:08:24 Saturday
-- brief:

module Main where

import Contravariant.Extras (contrazip2)
import qualified Control.Concurrent.Async as Async
import Control.Exception (SomeException, bracket, try)
import Control.Monad (join, replicateM_)
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Debug.Trace (traceShowM)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as TS
import System.Exit (exitFailure, exitSuccess)

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

-- `Session` 是在一次连接（查询）的上下文中执行的一批操作；
-- `Statement` 是严格的单语句查询的规范，可以被参数化以及提前预备（如何进行查询）；
-- `Statement` 由 SQL 模板，入参编码，返回值解码，以及是否准备好的标记所构成；
-- `statement` 通过 `Statement` 与入参创建一个 `Session`；
-- `run` 在提供的连接上，执行一批命令（statements）
cleanUp :: Connection.Connection -> IO (Either Session.SessionError ())
cleanUp connection = Session.run cleanUpSession connection
  where
    cleanUpSession :: Session.Session ()
    cleanUpSession = Session.statement () cleanUpStatement
    cleanUpStatement :: Statement.Statement () ()
    cleanUpStatement = Statement.Statement rawSql E.noParams D.noResult True
    rawSql = "truncate account"

createAccountTable :: Statement.Statement () ()
createAccountTable =
  Statement.Statement s E.noParams D.noResult False
  where
    s = "create table account (id serial not null, balance numeric not null, primary key (id))"

dropAccountTable :: Statement.Statement () ()
dropAccountTable =
  Statement.Statement "drop table account" E.noParams D.noResult False

createAccount :: Statement.Statement Scientific Int64
createAccount =
  Statement.Statement
    "insert into account (balance) values ($1) returning id"
    ((E.param . E.nonNullable) E.numeric)
    (D.singleRow ((D.column . D.nonNullable) D.int8))
    True

modifyBalance :: Statement.Statement (Int64, Scientific) Bool
modifyBalance =
  Statement.Statement
    "update account set balance = balance + $2 where id = $1"
    encode
    (fmap (> 0) D.rowsAffected)
    True
  where
    encode =
      contrazip2
        (E.param $ E.nonNullable E.int8)
        (E.param $ E.nonNullable E.numeric)

getBalance :: Statement.Statement Int64 (Maybe Scientific)
getBalance =
  Statement.Statement
    "select balance from account where id = $1"
    (E.param $ E.nonNullable E.int8)
    (D.rowMaybe ((D.column . D.nonNullable) D.numeric))
    True

----------------------------------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------------------------------

createSchema :: Transaction.Transaction ()
createSchema = Transaction.statement () createAccountTable

dropSchema :: Transaction.Transaction ()
dropSchema = Transaction.statement () dropAccountTable

transfer :: Int64 -> Int64 -> Scientific -> Transaction.Transaction Bool
transfer id1 id2 amount = do
  success <- Transaction.statement (id1, amount) modifyBalance
  if success
    then Transaction.statement (id2, negate amount) modifyBalance
    else return False

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> Transaction.Transaction ()
transferTimes times id1 id2 amount =
  replicateM_ times $ transfer id1 id2 amount

----------------------------------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------------------------------

session :: Connection.Connection -> Session.Session a -> IO a
session conn sess = Session.run sess conn >>= either (fail . show) return

transaction :: Connection.Connection -> Transaction.Transaction a -> IO a
transaction conn tran = session conn $ TS.transaction TS.RepeatableRead TS.Write tran

type Test = Connection.Connection -> Connection.Connection -> IO Bool

transactionsTest :: Test
transactionsTest conn1 conn2 = do
  id1 <- session conn1 $ Session.statement 0 createAccount
  id2 <- session conn1 $ Session.statement 0 createAccount
  async1 <- Async.async $ replicateM_ 1000 $ transaction conn1 $ transfer id1 id2 1
  async2 <- Async.async $ replicateM_ 1000 $ transaction conn2 $ transfer id1 id2 1
  Async.wait async1
  Async.wait async2
  balance1 <- session conn1 $ Session.statement id1 getBalance
  balance2 <- session conn1 $ Session.statement id2 getBalance
  traceShowM balance1
  traceShowM balance2

  return $ balance1 == Just 2000 && balance2 == Just (-2000)

readAndWriteTransactionsTest :: Test
readAndWriteTransactionsTest conn1 conn2 = do
  id1 <- session conn1 $ Session.statement 0 createAccount
  id2 <- session conn1 $ Session.statement 0 createAccount
  async1 <- Async.async $ replicateM_ 1000 $ transaction conn1 $ transfer id1 id2 1
  async2 <- Async.async $ replicateM_ 1000 $ transaction conn2 $ Transaction.statement id1 getBalance
  Async.wait async1
  Async.wait async2
  balance1 <- session conn1 $ Session.statement id1 getBalance
  balance2 <- session conn1 $ Session.statement id2 getBalance
  traceShowM balance1
  traceShowM balance2

  return $ balance1 == Just 1000 && balance2 == Just (-1000)

transactionAndQueryTest :: Test
transactionAndQueryTest conn1 conn2 = do
  id1 <- session conn1 $ Session.statement 0 createAccount
  id2 <- session conn1 $ Session.statement 0 createAccount
  async1 <- Async.async $ transaction conn1 $ transferTimes 200 id1 id2 1
  async2 <- Async.async $ session conn2 $ replicateM_ 200 $ Session.statement (id1, 1) modifyBalance
  Async.wait async1
  Async.wait async2
  balance1 <- session conn1 $ Session.statement id1 getBalance
  balance2 <- session conn1 $ Session.statement id2 getBalance
  traceShowM balance1
  traceShowM balance2

  return $ balance1 == Just 400 && balance2 == Just (-200)

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = bracket mAcquire mRelease mUse

----------------------------------------------------------------------------------------------------

mAcquire :: IO (Connection.Connection, Connection.Connection)
mAcquire = (,) <$> acq <*> acq
  where
    acq = join $ fmap (either (fail . show) return) $ Connection.acquire connectionSettings
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "1" "postgres"

mRelease :: (Connection.Connection, Connection.Connection) -> IO ()
mRelease (conn1, conn2) = do
  transaction conn1 dropSchema
  Connection.release conn1
  Connection.release conn2

mUse :: (Connection.Connection, Connection.Connection) -> IO b
mUse (conn1, conn2) = do
  _ <- try (transaction conn1 dropSchema) :: IO (Either SomeException ())
  transaction conn1 createSchema
  success <- fmap and (traverse runTest tests)
  if success then exitSuccess else exitFailure
  where
    runTest test = test conn1 conn2
    tests = [readAndWriteTransactionsTest, transactionsTest, transactionAndQueryTest]
