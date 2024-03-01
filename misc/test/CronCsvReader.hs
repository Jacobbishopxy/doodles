-- {-# LANGUAGE OverloadedStrings #-}

-- file: CronCsvReader.hs
-- author: Jacob Xie
-- date: 2024/03/01 10:52:44 Friday
-- brief:

module Main where

-- import Data. qualified as Alias
import Data.Csv

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: String
  }

main :: IO ()
main = do
  putStrLn "whatever"
