{-# LANGUAGE OverloadedStrings #-}

-- file: DirCsvReader.hs
-- author: Jacob Xie
-- date: 2024/03/02 09:44:41 Saturday
-- brief:

module Main where

import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Either (rights)
import Data.List (isSuffixOf)
import Data.Vector qualified as V
import System.Directory
import System.Directory.Internal.Prelude (getArgs, toLower)
import System.FilePath ((</>))

data CronSchema = CronSchema
  { idx :: Int,
    dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: Bool,
    fPath :: String
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
        { idx = 0,
          dag = dagVal,
          name = nameVal,
          sleeper = sleeperVal,
          input = inputVal,
          cmd = cmdVal,
          output = outputVal,
          activate = parseBool activateVal,
          fPath = ""
        }
    where
      parseBool = (== "true") . map toLower

-- discard header
readCsv :: FilePath -> IO (Either String [CronSchema])
readCsv file = do
  csvData <- BL.readFile file
  return $ processCsvData <$> decodeByName csvData
  where
    processCsvData :: (Header, V.Vector CronSchema) -> [CronSchema]
    processCsvData d = V.toList $ V.map (upfPath file) (V.indexed $ snd d)
    upfPath p (i, r) = r {fPath = p, idx = i + 1}

parseAllCsvFiles :: FilePath -> IO [CronSchema]
parseAllCsvFiles dir = do
  files <- filter (".csv" `isSuffixOf`) <$> getAbsDirectoryContents dir
  parsedData <- mapM readCsv files
  return . concat $ rights parsedData

-- turn relative filePath into filePath which based on execution location
getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = map (dir </>) <$> getDirectoryContents dir

main :: IO ()
main = do
  args <- getArgs
  -- dirs <- getAbsDirectoryContents $ head args
  -- mapM_ print dirs
  cronSchemas <- parseAllCsvFiles $ head args
  mapM_ print cronSchemas
