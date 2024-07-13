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

  -- get task task instances
  let reqT = mkReqLastN 5
      reqTi = mkReqTaskInstance "cronjob_monitor" reqT Nothing (Just [TsFailed])
  res1 <- R.run (getTaskInstance reqTi) connection
  print res1

  -- get failed celery tasks
  let reqT' = mkReqLastN 2
  res2 <- R.run (getFailedCeleryTask reqT') connection
  print res2

  -- get last n trade days run ids
  res3 <- R.run (getLastNTradeDaysRunId "cronjob_monitor" 30) connection
  print res3

  putStrLn "done"
