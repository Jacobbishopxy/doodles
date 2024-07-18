{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- file: SwaggerDemo.hs
-- author: Jacob Xie
-- date: 2024/07/15 13:29:17 Monday
-- brief:

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map
import Data.Swagger
import Data.Time
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Swagger.UI
import SwaggerApi

----------------------------------------------------------------------------------------------------

getTime :: Handler String
getTime = liftIO getCurrentTime >>= return . formatTime defaultTimeLocale "%FT%T%QZ"

type TodoStore = Map TodoId Todo

getTodos :: IORef TodoStore -> Handler [Todo]
getTodos = undefined

getTodo :: IORef TodoStore -> TodoId -> Handler Todo
getTodo = undefined

postTodo :: IORef TodoStore -> TodoId -> Handler TodoId
postTodo = undefined

putTodo :: IORef TodoStore -> TodoId -> Todo -> Handler Todo
putTodo = undefined

deleteTodo :: IORef TodoStore -> TodoId -> Handler Todo
deleteTodo = undefined

----------------------------------------------------------------------------------------------------

todoServer :: Server TodoAPI
todoServer = undefined

main :: IO ()
main = do
  let p = 8080 :: Int
  putStrLn $ "Starting server on port " <> show p