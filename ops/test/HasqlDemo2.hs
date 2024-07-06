{-# LANGUAGE OverloadedStrings #-}

-- file: HasqlDemo2.hs
-- author: Jacob Xie
-- date: 2024/07/05 22:48:42 Friday
-- brief: https://github.com/nikita-volkov/hasql-tutorial1/blob/master/library/HasqlTutorial1/Statement.hs

module Main where

import Contravariant.Extras.Contrazip
import Data.ByteString
import Data.Int (Int32)
import Data.Text
import Data.Vector
import qualified Hasql.Connection as C
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session (Session, run, statement)
import Hasql.Statement (Statement (..))

----------------------------------------------------------------------------------------------------
-- Statements
----------------------------------------------------------------------------------------------------

findUserByEmail :: Statement Text (Maybe Int32)
findUserByEmail =
  Statement
    "select id from user where email = $1"
    (E.param $ E.nonNullable E.text)
    (D.rowMaybe $ D.column $ D.nonNullable D.int4)
    True

insertUser :: Statement (Text, ByteString, Text, Maybe Text) Int32
insertUser =
  let s =
        "insert into user (email, password, name, phone) \
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
   in Statement s encoder decoder True

authenticateUser :: Statement (Text, ByteString) (Maybe (Bool, Int32))
authenticateUser =
  let s = "select password = $2, id from user where email = $1"
      encoder =
        contrazip2
          (E.param $ E.nonNullable E.text)
          (E.param $ E.nonNullable E.bytea)
      decoder =
        D.rowMaybe $
          (,)
            <$> D.column (D.nonNullable D.bool)
            <*> D.column (D.nonNullable D.int4)
   in Statement s encoder decoder True

getUserDetails :: Statement Int32 (Maybe (Text, Text, Maybe Text, Bool))
getUserDetails =
  let s = "select name, email, phone, admin from user where id = $1"
      encoder = E.param $ E.nonNullable E.int4
      decoder =
        D.rowMaybe $
          (,,,)
            <$> D.column (D.nonNullable D.text)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nullable D.text)
            <*> D.column (D.nonNullable D.bool)
   in Statement s encoder decoder True

getUserNotifications :: Statement Int32 (Vector (Int32, Text, Bool))
getUserNotifications =
  let s = "select id, message, read from notification where user = $1"
      encoder = E.param $ E.nonNullable E.int4
      decoder =
        D.rowVector $
          (,,)
            <$> D.column (D.nonNullable D.int4)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nonNullable D.bool)
   in Statement s encoder decoder True

markNotificationRead :: Statement Int32 Bool
markNotificationRead =
  Statement
    "update notification set read = true where id = $1"
    (E.param $ E.nonNullable E.int4)
    ((> 0) <$> D.rowsAffected)
    True

insertNotification :: Statement (Int32, Text) Int32
insertNotification =
  Statement
    "insert into notification (user, message, read)\
    \values ($1, $2, 'false')\
    \returning id"
    encoder
    decoder
    True
  where
    encoder = contrazip2 (E.param $ E.nonNullable E.int4) (E.param $ E.nonNullable E.text)
    decoder = D.singleRow (D.column $ D.nonNullable D.int4)

----------------------------------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------------------------------

-- tRegister :: Text -> ByteString -> Text -> Maybe Text -> T.Transaction (Bool, Int32)
-- tRegister email password name phone = session TS.Write TS.Serializable $ do
--   possibleExistingId <- statement email findUserByEmail
--   case possibleExistingId of
--     Just existingId -> return (False, existingId)
--     Nothing -> do
--       newId <- statement (email, password, name, phone) insertUser
--       return (True, newId)

-- tRegister' :: Text -> ByteString -> Text -> Maybe Text -> T.Transaction (Bool, Int32)
-- tRegister' email password name phone =
--   fromMaybeS
--     (fmap (\newId -> (True, newId)) (tInsertUser email password name phone))
--     (fmap (fmap (\existingId -> (False, existingId))) (tFindUserByEmail email))

-- tFindUserByEmail :: Text -> T.Transaction (Maybe Int32)
-- tFindUserByEmail  = undefined

-- tInsertUser :: Text -> ByteString -> Text -> Maybe Text -> T.Transaction Int32
-- tInsertUser email password name phone = undefined

----------------------------------------------------------------------------------------------------
-- Session
----------------------------------------------------------------------------------------------------

authenticate' :: Text -> ByteString -> Session (Maybe (Bool, Int32))
authenticate' email password = statement (email, password) authenticateUser

register :: Text -> ByteString -> Text -> Maybe Text -> Session (Bool, Int32)
register = undefined

getUserDetails' :: Int32 -> Session (Maybe (Text, Text, Maybe Text, Bool))
getUserDetails' userId = statement userId getUserDetails

getNotifications :: Int32 -> Session (Vector (Int32, Text, Bool))
getNotifications userId = statement userId getUserNotifications

markNotificationRead' :: Int32 -> Session Bool
markNotificationRead' notificationId = statement notificationId markNotificationRead

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  Right connection <- C.acquire cfg
  result <- run (authenticate' "foo@gmail.com" "abc123") connection
  print result
  where
    cfg = C.settings "localhost" 5432 "hasql" "hasql" "postgres"
