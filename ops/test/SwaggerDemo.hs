{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: SwaggerDemo.hs
-- author: Jacob Xie
-- date: 2024/07/15 13:29:17 Monday
-- brief:

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.Map as Map
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.IO.Handle (Handle)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Handler,
    Server,
    ServerError (errBody),
    err404,
    serve,
    throwError,
    (:<|>) ((:<|>)),
  )
import Servant.Swagger.UI (swaggerSchemaUIServer)
import SwaggerApi
  ( API,
    Todo,
    TodoAPI,
    TodoId (..),
    api,
    mkNewTodo,
    modifyTodo,
    swaggerDoc,
  )
import System.Info (os)
import System.Process (ProcessHandle, createProcess, shell)

----------------------------------------------------------------------------------------------------

getTime :: Handler String
getTime = liftIO getCurrentTime <&> formatTime defaultTimeLocale "%FT%T%QZ"

-- incremental id, mapping of id to `Todo`
type TodoStore = (Int, Map.Map TodoId Todo)

initTodoStore :: IO (IORef TodoStore)
initTodoStore = newIORef (0, Map.empty)

getAllTodo :: IORef TodoStore -> Handler [Todo]
getAllTodo rf =
  liftIO (readIORef rf) >>= \ts ->
    return $ snd <$> Map.toList (snd ts)

getTodo :: IORef TodoStore -> TodoId -> Handler Todo
getTodo rf i =
  liftIO (readIORef rf) >>= \ts ->
    case Map.lookup i (snd ts) of
      Just t -> return t
      Nothing -> throwError $ err404 {errBody = "todo not found"}

postTodo :: IORef TodoStore -> Todo -> Handler TodoId
postTodo rf t = do
  newT <- liftIO $ mkNewTodo t
  liftIO $ atomicModifyIORef rf $ \(i, m) ->
    let newI = i + 1
        newId = TodoId newI
        newM = Map.insert newId newT m
        newTs = (newI, newM)
     in (newTs, newId)

putTodo :: IORef TodoStore -> TodoId -> Todo -> Handler Todo
putTodo rf i t = do
  liftIO (readIORef rf) >>= \ts ->
    case Map.lookup i (snd ts) of
      Just oldTodo -> do
        updatedTodo <- liftIO $ modifyTodo t oldTodo
        liftIO $ atomicModifyIORef rf $ \(index, m) ->
          let newMap = Map.insert i updatedTodo m
           in ((index, newMap), ())
        return updatedTodo
      Nothing -> throwError $ err404 {errBody = "todo not found"}

deleteTodo :: IORef TodoStore -> TodoId -> Handler Todo
deleteTodo rf i = do
  liftIO (readIORef rf) >>= \ts ->
    case Map.lookup i (snd ts) of
      Just t -> do
        liftIO $ atomicModifyIORef rf $ \(index, m) ->
          let newMap = Map.delete i m
           in ((index, newMap), ())
        return t
      Nothing -> throwError $ err404 {errBody = "todo not found"}

----------------------------------------------------------------------------------------------------

todoServer :: IORef TodoStore -> Server TodoAPI
todoServer ref =
  getTime
    :<|> getAllTodo ref
    :<|> getTodo ref
    :<|> postTodo ref
    :<|> putTodo ref
    :<|> deleteTodo ref

server :: IORef TodoStore -> Server API
server ref = swaggerSchemaUIServer swaggerDoc :<|> todoServer ref

app :: IORef TodoStore -> Application
app ref = serve api (server ref)

launch :: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launch p =
  case os of
    "mingw32" -> createProcess (shell $ "start " ++ u)
    "darwin" -> createProcess (shell $ "open " ++ u)
    _ -> createProcess (shell $ "xdg-open " ++ u)
  where
    u = "http://localhost:" <> show p <> "/swagger-ui"

main :: IO ()
main = do
  let p = 8080 :: Int
  putStrLn $ "Starting server on port " <> show p
  ref <- initTodoStore
  _ <- launch p
  run p (app ref)
