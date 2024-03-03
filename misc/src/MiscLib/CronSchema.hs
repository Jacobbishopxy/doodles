{-# LANGUAGE OverloadedStrings #-}

-- file: CronSchema.hs
-- author: Jacob Xie
-- date: 2024/03/03 10:38:07 Sunday
-- brief:

module MiscLib.CronSchema
  ( CronSchema,
    searchAllCron,
    searchAllCrons,
  )
where

import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Csv (FromNamedRecord (..), Header, decodeByName, (.:))
import Data.Either (fromRight, rights)
import Data.List (isSuffixOf)
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
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
  return $ prc <$> decodeByName csvData
  where
    prc :: (Header, V.Vector CronSchema) -> [CronSchema]
    prc d = V.toList $ V.map (upc file) (V.indexed $ snd d)
    upc p (i, r) = r {fPath = p, idx = i + 1}

-- Given a directory, search all matched Csv files
searchCronByDir :: FilePath -> IO [CronSchema]
searchCronByDir dir = do
  files <- filter (".csv" `isSuffixOf`) <$> getAbsDirCtt dir
  parsedData <- mapM readCsv files
  return . concat $ rights parsedData
  where
    -- turn relative filePath into filePath which based on execution location
    getAbsDirCtt :: FilePath -> IO [FilePath]
    getAbsDirCtt d = map (dir </>) <$> getDirectoryContents d

-- Given a file, get `[CronSchema]`
searchCronByFile :: FilePath -> IO [CronSchema]
searchCronByFile file = do
  fromRight [] <$> readCsv file

----------------------------------------------------------------------------------------------------

-- search dir/file, discard none CronSchema Csv
searchAllCron :: FilePath -> IO [CronSchema]
searchAllCron fp = do
  isFile <- doesFileExist fp
  isDir <- doesDirectoryExist fp
  case (isFile, isDir) of
    (True, False) -> searchCronByFile fp
    (False, True) -> searchCronByDir fp
    _ -> return []

-- search multiple dirs/files
searchAllCrons :: [FilePath] -> IO [CronSchema]
searchAllCrons dirs = do
  ctts <- mapM searchAllCron dirs
  return $ concat ctts
