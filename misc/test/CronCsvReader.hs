{-# LANGUAGE OverloadedStrings #-}

-- file: CronCsvReader.hs
-- author: Jacob Xie
-- date: 2024/03/01 10:52:44 Friday
-- brief:

module Main where

import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Vector qualified as V
import System.Environment (getArgs)

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: String
  }
  deriving (Show)

instance FromNamedRecord CronSchema where
  parseNamedRecord m = do
    dagVal <- m .: "dag"
    nameVal <- m .: "name"
    sleeperVal <- m .: "sleeper"
    inputVal <- m .: "input"
    cmdVal <- m .: "cmd"
    outputVal <- m .: "output"
    activateVal <- m .: "activate"
    return
      CronSchema
        { dag = dagVal,
          name = nameVal,
          sleeper = sleeperVal,
          input = inputVal,
          cmd = cmdVal,
          output = outputVal,
          activate = activateVal
        }

type CsvData = (Header, V.Vector CronSchema)

readCsv :: FilePath -> IO (Either String CsvData)
readCsv file = do
  csvData <- BL.readFile file
  return $ decodeByName csvData

main :: IO ()
main = do
  args <- getArgs
  let csvFile = head args
  cronSchemas <- readCsv csvFile
  case cronSchemas of
    Left err -> putStrLn err
    Right (h, d) -> do
      putStrLn $ "Header: " <> show h
      mapM_ (\(i, r) -> putStrLn $ "Index " <> show i <> " : " <> show r) (V.indexed d)
