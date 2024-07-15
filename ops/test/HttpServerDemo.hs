{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: HttpServerDemo.hs
-- author: Jacob Xie
-- date: 2024/07/14 13:25:24 Sunday
-- brief:

module Main where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Decoding (eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status200, status201, status204, status400, status404, status405)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai (Application, Response, pathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)

data Todo = Todo
  { todoId :: Int,
    todoContent :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON Todo

instance FromJSON Todo

todosDir :: FilePath
todosDir = "./todos"

contentTypeJson :: [(HeaderName, ByteString)]
contentTypeJson = [("Content-Type", "application/json")]

contentTypeText :: [(HeaderName, ByteString)]
contentTypeText = [("Content-Type", "application/text")]

-- Helper to create a response
jsonResponse :: (ToJSON a) => Status -> a -> Response
jsonResponse status msg = responseLBS status contentTypeJson (encode msg)

-- Helper to read a todo from file
readTodo :: FilePath -> IO (Maybe Todo)
readTodo filePath = BL.readFile filePath >>= return . decode

-- Helper to write a todo to file
writeTodo :: Todo -> IO ()
writeTodo todo = BL.writeFile (todosDir <> "/" <> show (todoId todo) <> ".json") (encode todo)

-- Helper to delete a todo file
deleteTodo :: Int -> IO ()
deleteTodo i = removeFile $ todosDir <> "/" <> show i <> ".json"

-- app
app :: Application
app request respond =
  case (requestMethod request, pathInfo request) of
    ("GET", ["todos"]) ->
      listDirectory todosDir
        >>= mapM (readTodo . ((todosDir <> "/") <>))
        >>= respond . jsonResponse status200 . catMaybes
    ("GET", ["todos", todoIdText]) ->
      readTodo (todosDir <> "/" <> show todoIdText <> ".json")
        >>= maybe
          (respond $ jsonResponse status404 ("Todo not found" :: Text))
          (respond . jsonResponse status200)
    ("POST", ["todos"]) ->
      strictRequestBody request >>= \body ->
        case eitherDecode body of
          Left err -> respond $ jsonResponse status400 $ "Invalid JSON: " <> err
          Right todo -> writeTodo todo >> respond (jsonResponse status201 todo)
    ("PUT", ["todos", todoIdText]) -> do
      let tId = (read $ unpack todoIdText) :: Int
      fileExists <- doesFileExist $ todosDir <> "/" <> show tId <> ".json"
      if fileExists
        then
          strictRequestBody request >>= \body ->
            case eitherDecode body of
              Left err -> respond $ jsonResponse status400 $ "Invalid JSON" <> err
              Right todo -> writeTodo todo >> respond (jsonResponse status201 todo)
        else respond $ jsonResponse status404 ("Todo not found" :: Text)
    ("DELETE", ["todos", todoIdText]) -> do
      let tId = read $ unpack todoIdText
      fileExists <- doesFileExist $ todosDir <> "/" <> show tId <> ".json"
      if fileExists
        then deleteTodo tId >> respond (responseLBS status204 contentTypeText "")
        else respond $ jsonResponse status404 ("Todo not found" :: Text)
    ("DELETE", ["todos"]) ->
      listDirectory todosDir
        >>= mapM_ (removeFile . ((todosDir <> "/") <>))
        >> respond (responseLBS status204 contentTypeText "")
    _ -> respond $ responseLBS status405 contentTypeText "Method Not Allowed"

-- Ensure to todos directory exists
createDirIfMissing :: IO ()
createDirIfMissing = do
  dirExists <- doesFileExist todosDir
  unless dirExists $ createDirectoryIfMissing True todosDir

-- main
main :: IO ()
main = do
  _ <- createDirIfMissing

  let port = 8080 :: Int
  putStrLn $ "Starting server on port " <> show port
  run port app
