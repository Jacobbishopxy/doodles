{-# LANGUAGE OverloadedStrings #-}

-- file: AfTasks.hs
-- author: Jacob Xie
-- date: 2024/07/07 21:57:05 Sunday
-- brief:

module Main where

import Configuration.Dotenv (Config (configPath), defaultConfig, loadFile)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import qualified Hasql.Connection as C
import qualified Hasql.Session as R
import OpsLib.AfTasks
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  -- load dotenv file
  loadFile defaultConfig {configPath = ["./ops/test/AfTasks.env"]}
  -- load vars
  afHost <- lookupEnv "AF_HOST"
  afPort <- lookupEnv "AF_PORT"
  afUser <- lookupEnv "AF_USER"
  afPass <- lookupEnv "AF_PASS"
  afDb <- lookupEnv "AF_DB"

  let cfg =
        C.settings
          (pack $ fromMaybe "loaclhost" afHost)
          (fromMaybe 5432 $ afPort >>= readMaybe)
          (pack $ fromMaybe "postgres" afUser)
          (pack $ fromMaybe "1" afPass)
          (pack $ fromMaybe "postgres" afDb)

  Right connection <- C.acquire cfg

  -- res1 <- R.run (getTaskInstancesByStates "cronjob_trade" "20240708" [TsFailed]) connection
  -- print res1

  -- res2 <- R.run (getFailedCeleryTask (mkFromTo "%Y%m%d" ("20240709", "20240710"))) connection
  -- res2 <- R.run (getFailedCeleryTask (mkFrom "%Y%m%d" "20240710")) connection
  -- print res2

  res3 <- R.run (getLastNTradeDaysRunId "cronjob_monitor" 30) connection
  print res3

  putStrLn "done"
