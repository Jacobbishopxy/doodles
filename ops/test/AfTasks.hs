{-# LANGUAGE OverloadedStrings #-}

-- file: AfTasks.hs
-- author: Jacob Xie
-- date: 2024/07/07 21:57:05 Sunday
-- brief:

module Main where

import qualified Hasql.Connection as C
import qualified Hasql.Session as R
import OpsLib.AfTasks

main :: IO ()
main = do
  Right connection <- C.acquire cfg

  -- res1 <- R.run (getTaskInstancesByStates "cronjob_trade" "20240708" [TsFailed]) connection
  -- print res1

  -- res2 <- R.run (getFailedCeleryTask (mkFromTo "%Y%m%d" ("20240709", "20240710"))) connection
  -- res2 <- R.run (getFailedCeleryTask (mkFrom "%Y%m%d" "20240710")) connection
  -- print res2

  res3 <- R.run (getLastNTradeDaysRunId "cronjob_monitor" 10) connection
  print res3

  putStrLn "done"

----------------------------------------------------------------------------------------------------

cfg :: C.Settings
-- cfg = C.settings "localhost" 5432 "postgres" "1" "hasql"
cfg = C.settings "10.144.66.43" 5433 "postgres" "1" "airflow"
