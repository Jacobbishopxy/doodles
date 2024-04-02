{-# LANGUAGE OverloadedStrings #-}

-- file: CronSchema.hs
-- author: Jacob Xie
-- date: 2024/03/03 10:38:07 Sunday
-- brief:

module MiscLib.CronSchema
  ( CronSchema,
    Conj (..),
    SearchParam (..),
    getAllCron,
    getAllCrons,
    searchCron,
  )
where

import Data.Either (fromRight, rights)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import MiscLib.CsvHelper
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

-- CronSchema
data CronSchema = CronSchema
  { idx :: Int,
    dag :: String,
    name :: String,
    sleeper :: String,
    input :: Maybe String,
    cmd :: String,
    output :: Maybe String,
    activate :: Bool,
    fPath :: String
  }
  deriving (Show)

-- Conjunction
data Conj = AND | OR deriving (Enum, Read, Eq, Ord)

-- SearchParam
data SearchParam = SearchParam
  { searchFields :: [String],
    searchConj :: Conj,
    searchStr :: String
  }

----------------------------------------------------------------------------------------------------
-- Impl
----------------------------------------------------------------------------------------------------

instance ParseRecord CronSchema where
  parseRecord header row =
    CronSchema
      { idx = 0,
        dag = readString header row "dag",
        name = readString header row "name",
        sleeper = readString header row "sleeper",
        input = readString' header row "input",
        cmd = readString header row "cmd",
        output = readString' header row "output",
        activate = readBool header row "activate",
        fPath = ""
      }

----------------------------------------------------------------------------------------------------
-- Fn
----------------------------------------------------------------------------------------------------

-- search dir/file, discard none CronSchema Csv
getAllCron :: FilePath -> IO [CronSchema]
getAllCron fp = do
  isFile <- doesFileExist fp
  isDir <- doesDirectoryExist fp
  case (isFile, isDir) of
    (True, False) -> searchCronByFile fp
    (False, True) -> searchCronByDir fp
    _ -> return []

-- search multiple dirs/files
getAllCrons :: [FilePath] -> IO [CronSchema]
getAllCrons dirs = concat <$> mapM getAllCron dirs

-- search `[CronSchema]` contents
searchCron :: SearchParam -> [CronSchema] -> [CronSchema]
searchCron sp = filter $ containsSubstring conj lookupStr . flip getCronStrings fields
  where
    fields = searchFields sp
    conj = searchConj sp
    lookupStr = searchStr sp

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

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
searchCronByFile file = fromRight [] <$> readCsv file

-- Get all strings by a field list
getCronStrings :: CronSchema -> [String] -> [String]
getCronStrings cron = map f
  where
    f "dag" = dag cron
    f "name" = name cron
    f "sleeper" = sleeper cron
    f "input" = fromMaybe "" $ input cron
    f "cmd" = cmd cron
    f "output" = fromMaybe "" $ output cron
    f _ = ""

-- Check a list of string contain a substring
containsSubstring :: Conj -> String -> [String] -> Bool
containsSubstring conj lookupStr = f (lookupStr `isInfixOf`)
  where
    f = case conj of
      AND -> all
      OR -> any
