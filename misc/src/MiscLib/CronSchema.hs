{-# LANGUAGE OverloadedStrings #-}

-- file: CronSchema.hs
-- author: Jacob Xie
-- date: 2024/03/03 10:38:07 Sunday
-- brief:

module MiscLib.CronSchema
  ( CronSchema,
    Conj,
    SearchParam,
    getAllCron,
    getAllCrons,
    genSearchParam,
    searchCron,
  )
where

import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Csv (FromNamedRecord (..), Header, decodeByName, (.:))
import Data.Either (fromRight, rights)
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Vector as V
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

-- Conjunction
data Conj = AND | OR deriving (Enum, Read)

-- SearchParam
data SearchParam = SearchParam
  { searchFields :: [String],
    searchConj :: Conj,
    searchStr :: String
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

genSearchParam :: [String] -> Conj -> String -> SearchParam
genSearchParam f c s =
  SearchParam
    { searchFields = f,
      searchConj = c,
      searchStr = s
    }

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
searchCronByFile file = fromRight [] <$> readCsv file

-- Get all strings by a field list
getCronStrings :: CronSchema -> [String] -> [String]
getCronStrings cron = map f
  where
    f "dag" = dag cron
    f "name" = name cron
    f "sleeper" = sleeper cron
    f "input" = input cron
    f "cmd" = cmd cron
    f "output" = output cron
    f _ = ""

-- Check a list of string contain a substring
containsSubstring :: Conj -> String -> [String] -> Bool
containsSubstring conj lookupStr = f (lookupStr `isInfixOf`)
  where
    f = case conj of
      AND -> all
      OR -> any
