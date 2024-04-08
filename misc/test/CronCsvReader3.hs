{-# LANGUAGE OverloadedStrings #-}

-- file: CronCsvReader3.hs
-- author: Jacob Xie
-- date: 2024/03/31 15:38:54 Sunday
-- brief:

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.List (elemIndex)
import System.Environment (getArgs)
import System.IO

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
  handle <- openFile file ReadMode
  contents <- C.hGetContents handle

  case C.lines contents of
    (headerLine : bodyLines) -> do
      let header = rowSplit headerLine
      return [parseRecord header (rowSplit x) | x <- bodyLines]
    _ -> error "Insufficient lines in CSV file"

----------------------------------------------------------------------------------------------------

rowSplit :: ByteString -> [ByteString]
rowSplit bs = f bs [] [] 0
  where
    f :: ByteString -> String -> [ByteString] -> Int -> [ByteString]
    f s cur line _ | C.length s == 1 = reverse (C.pack (reverse cur) : line)
    f s cur line n | C.head s == ',' && even n = f (C.tail s) [] (C.pack (reverse cur) : line) 0
    f s cur line n = f (C.tail s) (C.head s : cur) line (if C.head s == '"' then n + 1 else n)

parseRecord :: [ByteString] -> [ByteString] -> CronSchema
parseRecord header row =
  CronSchema
    { dag = C.unpack $ g "dag",
      name = C.unpack $ g "name",
      sleeper = C.unpack $ g "sleeper",
      input = C.unpack $ g "input",
      cmd = C.unpack $ g "cmd",
      output = C.unpack $ g "output",
      activate = readBool $ g "activate",
      retries = readFloat $ g "retries"
    }
  where
    g = getField header row
    readFloat :: ByteString -> Float
    readFloat s = case reads (C.unpack s) of
      [(x, "")] -> x
      _ -> 0
    readBool :: ByteString -> Bool
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
