-- file: CronCsvReader.hs
-- author: Jacob Xie
-- date: 2024/03/01 10:52:44 Friday
-- brief:

module Main where

import MiscLib.CronSchema
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let csvFile = head args
  crons <- getAllCron csvFile

  print crons
