{-# LANGUAGE OverloadedStrings #-}

-- file: ShowCsv2.hs
-- author: Jacob Xie
-- date: 2024/03/31 20:26:56 Sunday
-- brief:

module Main where

import MiscLib.CsvHelper
import System.Environment (getArgs)

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: Bool,
    retries :: Int
  }
  deriving (Show)

instance ParseRecord CronSchema where
  parseRecord header row =
    CronSchema
      { dag = readString header row "dag",
        name = readString header row "name",
        sleeper = readString header row "sleeper",
        input = readString header row "input",
        cmd = readString header row "cmd",
        output = readString header row "output",
        activate = readBool header row "activate",
        retries = readInt header row "retries"
      }

main :: IO ()
main = do
  args <- getArgs
  let csvFile = head args
  cronSchemas <- readCsv csvFile

  case cronSchemas of
    Right cs -> mapM_ (\(i, r) -> putStrLn $ "Index " <> show (i :: Int) <> " : " <> show r) (zip [0 ..] (cs :: [CronSchema]))
    _ -> error "Insufficient lines in CSV file"
