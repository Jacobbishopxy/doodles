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
import qualified Hasql.Connection as C
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as R
import qualified Hasql.Statement as S
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as TS
import System.Exit (exitFailure, exitSuccess)

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

createAccountTable :: S.Statement () ()
createAccountTable =
  S.Statement s E.noParams D.noResult False
  where
    s = "create table account (id serial not null, balance numeric not null, primary key (id))"

dropAccountTable :: S.Statement () ()
dropAccountTable =
  S.Statement "drop table account" E.noParams D.noResult False

createAccount :: S.Statement Scientific Int64
createAccount =
  S.Statement
    "insert into account (balance) values ($1) returning id"
    ((E.param . E.nonNullable) E.numeric)
    (D.singleRow ((D.column . D.nonNullable) D.int8))
    True

modifyBalance :: S.Statement (Int64, Scientific) Bool
modifyBalance =
  S.Statement
    "update account set balance = balance + $2 where id = $1"
    encode
    (fmap (> 0) D.rowsAffected)
    True
  where
    encode =
      contrazip2
        (E.param $ E.nonNullable E.int8)
        (E.param $ E.nonNullable E.numeric)

getBalance :: S.Statement Int64 (Maybe Scientific)
getBalance =
  S.Statement
    "select balance from account where id = $1"
    (E.param $ E.nonNullable E.int8)
    (D.rowMaybe ((D.column . D.nonNullable) D.numeric))
    True

----------------------------------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------------------------------

createSchema :: T.Transaction ()
createSchema = T.statement () createAccountTable

dropSchema :: T.Transaction ()
dropSchema = T.statement () dropAccountTable

transfer :: Int64 -> Int64 -> Scientific -> T.Transaction Bool
transfer id1 id2 amount = do
  success <- T.statement (id1, amount) modifyBalance
  if success
    then T.statement (id2, negate amount) modifyBalance
    else return False

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> T.Transaction ()
transferTimes times id1 id2 amount =
  replicateM_ times $ transfer id1 id2 amount

----------------------------------------------------------------------------------------------------
-- Test
----------------------------------------------------------------------------------------------------

session :: C.Connection -> R.Session a -> IO a
session conn sess = R.run sess conn >>= either (fail . show) return

transaction :: C.Connection -> T.Transaction a -> IO a
transaction conn tran = session conn $ TS.transaction TS.RepeatableRead TS.Write tran

type Test = C.Connection -> C.Connection -> IO Bool

transactionsTest :: Test
transactionsTest conn1 conn2 = do
  id1 <- session conn1 $ R.statement 0 createAccount
  id2 <- session conn1 $ R.statement 0 createAccount
  async1 <- Async.async $ replicateM_ 1000 $ transaction conn1 $ transfer id1 id2 1
  async2 <- Async.async $ replicateM_ 1000 $ transaction conn2 $ transfer id1 id2 1
  Async.wait async1
  Async.wait async2
  balance1 <- session conn1 $ R.statement id1 getBalance
  balance2 <- session conn1 $ R.statement id2 getBalance
  traceShowM balance1
  traceShowM balance2

  return $ balance1 == Just 2000 && balance2 == Just (-2000)

readAndWriteTransactionsTest :: Test
readAndWriteTransactionsTest conn1 conn2 = do
  id1 <- session conn1 $ R.statement 0 createAccount
  id2 <- session conn1 $ R.statement 0 createAccount
  async1 <- Async.async $ replicateM_ 1000 $ transaction conn1 $ transfer id1 id2 1
  async2 <- Async.async $ replicateM_ 1000 $ transaction conn2 $ T.statement id1 getBalance
  Async.wait async1
  Async.wait async2
  balance1 <- session conn1 $ R.statement id1 getBalance
  balance2 <- session conn1 $ R.statement id2 getBalance
  traceShowM balance1
  traceShowM balance2

  return $ balance1 == Just 1000 && balance2 == Just (-1000)

transactionAndQueryTest :: Test
transactionAndQueryTest conn1 conn2 = do
  id1 <- session conn1 $ R.statement 0 createAccount
  id2 <- session conn1 $ R.statement 0 createAccount
  async1 <- Async.async $ transaction conn1 $ transferTimes 200 id1 id2 1
  async2 <- Async.async $ session conn2 $ replicateM_ 200 $ R.statement (id1, 1) modifyBalance
  Async.wait async1
  Async.wait async2
  balance1 <- session conn1 $ R.statement id1 getBalance
  balance2 <- session conn1 $ R.statement id2 getBalance
  traceShowM balance1
  traceShowM balance2

  return $ balance1 == Just 400 && balance2 == Just (-200)

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = bracket mAcquire mRelease mUse

----------------------------------------------------------------------------------------------------

mAcquire :: IO (C.Connection, C.Connection)
mAcquire = (,) <$> acq <*> acq
  where
    acq = join $ fmap (either (fail . show) return) $ C.acquire cfg
    cfg = C.settings "localhost" 5432 "postgres" "1" "postgres"

mRelease :: (C.Connection, C.Connection) -> IO ()
mRelease (conn1, conn2) = do
  transaction conn1 dropSchema
  C.release conn1
  C.release conn2

mUse :: (C.Connection, C.Connection) -> IO b
mUse (conn1, conn2) = do
  _ <- try (transaction conn1 dropSchema) :: IO (Either SomeException ())
  transaction conn1 createSchema
  success <- fmap and (traverse runTest tests)
  if success then exitSuccess else exitFailure
  where
    runTest test = test conn1 conn2
    tests = [readAndWriteTransactionsTest, transactionsTest, transactionAndQueryTest]
