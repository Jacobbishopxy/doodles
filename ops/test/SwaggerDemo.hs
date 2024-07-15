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
import Data.Swagger
import Data.Text
import Data.Time
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Swagger.UI
import SwaggerApi

main :: IO ()
main = do
  let p = 8080 :: Int
  putStrLn $ "Starting server on port " <> show p