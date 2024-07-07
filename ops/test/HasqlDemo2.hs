{-# LANGUAGE OverloadedStrings #-}

-- file: HasqlDemo2.hs
-- author: Jacob Xie
-- date: 2024/07/05 22:48:42 Friday
-- brief: https://github.com/nikita-volkov/hasql-tutorial1/blob/master/library/HasqlTutorial1/Statement.hs

module Main where

import Contravariant.Extras.Contrazip (contrazip2, contrazip4)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Text (Text)
import Data.Vector (Vector, fromList, toList)
import qualified Data.Vector as V
import qualified Hasql.Connection as C
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as R
import qualified Hasql.Statement as S
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as TS

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

simpleExec :: ByteString -> S.Statement () ()
simpleExec s =
  S.Statement s E.noParams D.noResult True

-- `Session` 是在一次连接（查询）的上下文中执行的一批操作；
-- `Statement` 是严格的单语句查询的规范，可以被参数化以及提前预备（如何进行查询）；
-- `Statement` 由 SQL 模板，入参编码，返回值解码，以及是否准备好的标记所构成；
-- `statement` 通过 `Statement` 与入参创建一个 `Session`；
-- `run` 在提供的连接上，执行一批命令（statements）
cleanUp :: C.Connection -> IO (Either R.SessionError ())
cleanUp connection = R.run cleanUpSession connection
  where
    cleanUpSession :: R.Session ()
    cleanUpSession = R.statement () cleanUpStatement
    cleanUpStatement :: S.Statement () ()
    cleanUpStatement = S.Statement rawSql E.noParams D.noResult True
    rawSql = "truncate \"user\", notification"

-- 根据 Email 查找用户
findUserByEmail :: S.Statement Text (Maybe Int32)
findUserByEmail =
  S.Statement
    "select id from \"user\" where email = $1"
    (E.param $ E.nonNullable E.text)
    (D.rowMaybe $ D.column $ D.nonNullable D.int4)
    True

-- 插入用户
insertUser :: S.Statement (Text, ByteString, Text, Maybe Text) Int32
insertUser =
  let s =
        "insert into \"user\" (email, password, name, phone) \
        \values ($1, $2, $3, $4) \
        \returning id"
      encoder =
        contrazip4
          (E.param $ E.nonNullable E.text)
          (E.param $ E.nonNullable E.bytea)
          (E.param $ E.nonNullable E.text)
          (E.param $ E.nullable E.text)
      decoder =
        D.singleRow $ (D.column . D.nonNullable) D.int4
   in S.Statement s encoder decoder True

-- 通过 Email 与 password 验证用户
authenticateUser :: S.Statement (Text, ByteString) (Maybe (Bool, Int32))
authenticateUser =
  let s = "select password = $2, id from \"user\" where email = $1"
      encoder =
        contrazip2
          (E.param $ E.nonNullable E.text)
          (E.param $ E.nonNullable E.bytea)
      decoder =
        D.rowMaybe $
          (,)
            <$> D.column (D.nonNullable D.bool)
            <*> D.column (D.nonNullable D.int4)
   in S.Statement s encoder decoder True

-- 获取用户明细
getUserDetails :: S.Statement Text (Maybe (Int32, Text, Text, Maybe Text, Maybe Bool))
getUserDetails =
  let s = "select id, name, email, phone, admin from \"user\" where email = $1"
      encoder = E.param $ E.nonNullable E.text
      decoder =
        D.rowMaybe $
          (,,,,)
            <$> D.column (D.nonNullable D.int4)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nullable D.text)
            <*> D.column (D.nullable D.bool)
   in S.Statement s encoder decoder True

-- 插入通知
insertNotification :: S.Statement (Int32, Text) Int32
insertNotification =
  S.Statement
    "insert into notification (\"user\", message, read)\
    \values ($1, $2, 'false')\
    \returning id"
    encoder
    decoder
    True
  where
    encoder = contrazip2 (E.param $ E.nonNullable E.int4) (E.param $ E.nonNullable E.text)
    decoder = D.singleRow (D.column $ D.nonNullable D.int4)

-- 获取通知
getNotifications :: S.Statement Int32 (Vector (Int32, Text, Bool))
getNotifications =
  let s = "select id, message, read from notification where \"user\" = $1"
      encoder = E.param $ E.nonNullable E.int4
      decoder =
        D.rowVector $
          (,,)
            <$> D.column (D.nonNullable D.int4)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nonNullable D.bool)
   in S.Statement s encoder decoder True

-- 将通知设为已读
markNotificationRead :: S.Statement Int32 Bool
markNotificationRead =
  S.Statement
    "update notification set read = true where id = $1"
    (E.param $ E.nonNullable E.int4)
    ((> 0) <$> D.rowsAffected)
    True

-- 将多个通知设为已读
markNotificationsRead :: S.Statement (Vector Int32) (Vector Int32)
markNotificationsRead =
  S.Statement
    "update notification set read = true where id = any ($1) returning id"
    (E.param $ E.nonNullable $ E.array $ E.dimension List.foldl' (E.element $ E.nonNullable E.int4))
    (D.rowVector $ D.column (D.nonNullable D.int4))
    True

----------------------------------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------------------------------

-- 初始化
initTables :: T.Transaction ()
initTables = do
  _ <- T.statement () (simpleExec t1)
  _ <- T.statement () (simpleExec t2)
  _ <- T.statement () (simpleExec t3)

  return ()
  where
    t1 =
      "create table \"user\" (\
      \id serial not null primary key, \
      \email text not null unique, \
      \password bytea not null, \
      \name text not null, \
      \phone text null, \
      \admin bool null);"
    t2 =
      "create table notification (\
      \id serial not null primary key, \
      \\"user\" int4 not null references \"user\", \
      \message text not null, \
      \read bool not null);"
    t3 = "create index \"notification_user\" on \"notification\" (\"user\");"

-- 注册用户
-- 已存在的 Email 不可注册用户
registerUser :: Text -> ByteString -> Text -> Maybe Text -> T.Transaction (Bool, Int32)
registerUser email password name phone = do
  possibleExistingId <- tFindUserByEmail email
  case possibleExistingId of
    Just existingId -> return (False, existingId)
    Nothing -> c >>= \i -> return (True, i)
  where
    c :: T.Transaction Int32
    c = T.statement (email, password, name, phone) insertUser

-- 推送通知
-- 不存在的 Email 不可推送
pushUserNotification :: Text -> Text -> T.Transaction Bool
pushUserNotification email notification = do
  possibleExistingId <- tFindUserByEmail email
  case possibleExistingId of
    Just existingId -> c existingId >> return True
    Nothing -> return False
  where
    c :: Int32 -> T.Transaction Int32
    c i = T.statement (i, notification) insertNotification

-- 用户通知已读
markUserNotificationsRead :: Text -> [Int32] -> T.Transaction [Int32]
markUserNotificationsRead email notificationIds = do
  possibleExistingId <- tFindUserByEmail email
  case possibleExistingId of
    Just _ -> c >>= \i -> return $ toList i
    Nothing -> return []
  where
    c :: T.Transaction (Vector Int32)
    c = T.statement (fromList notificationIds) markNotificationsRead

----------------------------------------------------------------------------------------------------

-- 带有 transaction 的查找用户
tFindUserByEmail :: Text -> T.Transaction (Maybe Int32)
tFindUserByEmail email = T.statement email findUserByEmail

----------------------------------------------------------------------------------------------------
-- Session
-- 执行函数，业务意义
----------------------------------------------------------------------------------------------------

initAll :: R.Session ()
initAll = TS.transaction TS.ReadCommitted TS.Write initTables

authenticate :: Text -> ByteString -> R.Session (Maybe (Bool, Int32))
authenticate email password = R.statement (email, password) authenticateUser

register :: Text -> ByteString -> Text -> Maybe Text -> R.Session (Bool, Int32)
register email password name phone =
  TS.transaction
    TS.ReadCommitted
    TS.Write
    (registerUser email password name phone)

getDetails :: Text -> R.Session (Maybe (Int32, Text, Text, Maybe Text, Maybe Bool))
getDetails email = R.statement email getUserDetails

pushNotification :: Text -> Text -> R.Session Bool
pushNotification email notification =
  TS.transaction
    TS.ReadCommitted
    TS.Write
    (pushUserNotification email notification)

getAllNotifications :: Int32 -> R.Session (Vector (Int32, Text, Bool))
getAllNotifications userId = R.statement userId getNotifications

markNotifications :: Text -> [Int32] -> R.Session [Int32]
markNotifications email notificationIds =
  TS.transaction
    TS.RepeatableRead
    TS.Write
    (markUserNotificationsRead email notificationIds)

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  Right connection <- C.acquire cfg

  -- 初始化
  _ <- R.run initAll connection

  let uEmail = "foo@gmail.com"
      uPswd = "abc123"
      uName = "foo"
      uPhone = "360100100"

  -- 尝试验证一个用户，发现该用户并未被注册
  res1 <- R.run (authenticate uEmail uPswd) connection
  print res1

  -- 注册用户
  res2 <- R.run (register uEmail uPswd uName (Just uPhone)) connection
  print res2

  -- 通过 Email 查找用户明细
  res3 <- R.run (getDetails uEmail) connection
  print res3
  id' <- case res3 of
    Right (Just d@(i, _, _, _, _)) -> do print d; return i
    _ -> error "failed to get the user detail!"

  -- 向用户推送通知
  _ <- R.run (pushNotification uEmail "Hello world") connection
  _ <- R.run (pushNotification uEmail "Hello Jacob") connection

  -- 通过 id 获取用户通知列表
  res4 <- R.run (getAllNotifications id') connection
  print res4

  -- 获取所有通知 ids
  nIds <- case res4 of
    Right v -> return $ V.toList $ V.map (\(x, _, _) -> x) v
    _ -> return []

  -- 将所有通知设为已读
  res5 <- R.run (markNotifications uEmail nIds) connection
  print res5

  putStrLn "done"

----------------------------------------------------------------------------------------------------

cfg :: C.Settings
cfg = C.settings "localhost" 5432 "postgres" "1" "hasql"
