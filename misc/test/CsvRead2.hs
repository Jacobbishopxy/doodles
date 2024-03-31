{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead2.hs
-- author: Jacob Xie
-- date: 2024/03/31 19:34:58 Sunday
-- brief:

module Main where

import Data.List (elemIndex)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: Bool,
    retries :: Float
  }
  deriving (Show)

type CsvData = [CronSchema]

readCsv :: FilePath -> IO CsvData
readCsv file = do
  contents <- TIO.readFile file

  case T.lines contents of
    (headerLine : bodyLines) ->
      return [parseRecord (rowSplit headerLine) (rowSplit x) | x <- bodyLines]
    _ -> error "Insufficient lines in CSV file"

rowSplit :: T.Text -> [T.Text]
rowSplit = T.splitOn "," . T.strip

----------------------------------------------------------------------------------------------------

parseRecord :: [T.Text] -> [T.Text] -> CronSchema
parseRecord header row =
  CronSchema
    { dag = T.unpack $ g "dag",
      name = T.unpack $ g "name",
      sleeper = T.unpack $ g "sleeper",
      input = T.unpack $ g "input",
      cmd = T.unpack $ g "cmd",
      output = T.unpack $ g "output",
      activate = readBool $ g "activate",
      retries = readFloat $ g "retries"
    }
  where
    g = getField header row
    readFloat :: T.Text -> Float
    readFloat s = case reads (T.unpack s) of
      [(x, "")] -> x
      _ -> 0
    readBool :: T.Text -> Bool
    readBool "TRUE" = True
    readBool _ = False

getField :: (Eq a) => [a] -> [b] -> a -> b
getField headers row name = case name `elemIndex` headers of
  Nothing -> error "Element not in list"
  Just i -> row !! i

main :: IO ()
main = do
  args <- getArgs
  let csvFile = head args
  cronSchemas <- readCsv csvFile

  print cronSchemas
