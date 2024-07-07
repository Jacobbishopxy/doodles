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

  res <- R.run (getTaskInstance "cronjob_monitor" "20230728" "start") connection
  print res

  putStrLn "done"

----------------------------------------------------------------------------------------------------

cfg :: C.Settings
cfg = C.settings "localhost" 5432 "postgres" "1" "hasql"
