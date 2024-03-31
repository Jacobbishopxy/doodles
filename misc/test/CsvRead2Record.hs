{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead2Record.hs
-- author: Jacob Xie
-- date: 2024/03/31 15:38:54 Sunday
-- brief:



module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, isNothing)
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
    (headerLine : bodyLine) -> do
      let header = rowSplit headerLine
      return [parseRecord header (rowSplit x) | x <- bodyLine]
    _ -> error "Insufficient lines in CSV file"

----------------------------------------------------------------------------------------------------

-- reorder fields based on header
reorderFields :: [ByteString] -> [ByteString] -> [ByteString]
reorderFields header fields = map (fromMaybe "" . flip lookup indexedFields) [0 .. length header - 1]
  where
    indexedFields = zip [0 ..] fields

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
    { dag = C.unpack $ getField "dag" header row,
      name = C.unpack $ getField "name" header row,
      sleeper = C.unpack $ getField "sleeper" header row,
      input = C.unpack $ getField "input" header row,
      cmd = C.unpack $ getField "cmd" header row,
      output = C.unpack $ getField "output" header row,
      activate = readBool $ getField "activate" header row,
      retries = readFloat $ getField "retries" header row
    }
  where
    readFloat :: ByteString -> Float
    readFloat s = case reads (C.unpack s) of
      [(x, "")] -> x
      _ -> 0
    readBool :: ByteString -> Bool
    readBool "TRUE" = True
    readBool _ = False

-- find index
(!!?) :: (Eq a) => [a] -> [a] -> Either String [Int]
(!!?) xs ys
  | any isNothing f = Left "Some elements not found in the list"
  | otherwise = Right $ map fromJust f
  where
    f = map (`elemIndex` ys) xs

getField :: (Eq a) => a -> [a] -> [b] -> b
getField name headers row = case name `elemIndex` headers of
  Nothing -> error "Element not in list"
  Just i -> row !! i

main :: IO ()
main = do
  args <- getArgs
  let csvFile = head args
  cronSchemas <- readCsv csvFile

  print cronSchemas
