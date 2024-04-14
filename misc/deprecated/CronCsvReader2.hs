{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead2.hs
-- author: Jacob Xie
-- date: 2024/03/31 19:34:58 Sunday
-- brief:

module Main where

import Data.ByteString.Lazy qualified as BS
import Data.List (elemIndex)
import Data.Text qualified as T
import Data.Text.ICU.Convert qualified as ICU
import System.Environment (getArgs)
import System.IO qualified as SIO

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: Bool
    -- retries :: Float
  }
  deriving (Show)

type CsvData = [CronSchema]

readCsv :: FilePath -> IO CsvData
readCsv file = do
  -- Open the file in binary mode
  fileHandle <- SIO.openBinaryFile file SIO.ReadMode
  -- Read the file contents as ByteString
  fileBytes <- BS.hGetContents fileHandle

  -- Create a converter for converting from GBK to UTF-8
  converter <- ICU.open "utf-8" Nothing
  -- Convert the ByteString contents from GBK to UTF-8 encoded Text
  let decodedContents = ICU.toUnicode converter (BS.toStrict fileBytes)

  case T.lines decodedContents of
    (headerLine : bodyLines) ->
      return [parseRecord (rowSplit headerLine) (rowSplit x) | x <- bodyLines]
    _ -> error "Insufficient lines in CSV file"

rowSplit :: T.Text -> [T.Text]
rowSplit t = f t [] [] 0
  where
    f :: T.Text -> String -> [T.Text] -> Int -> [T.Text]
    f s cur line _ | T.length s == 1 = reverse (T.pack (reverse cur) : line)
    f s cur line n | T.head s == ',' && even n = f (T.tail s) [] (T.pack (reverse cur) : line) 0
    f s cur line n = f (T.tail s) (T.head s : cur) line (if T.head s == '"' then n + 1 else n)

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
      activate = readBool $ g "activate"
    }
  where
    g = getField header row
    -- readFloat :: T.Text -> Float
    -- readFloat s = case reads (T.unpack s) of
    --   [(x, "")] -> x
    --   _ -> 0
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

  mapM_ (\(i, r) -> putStrLn $ "Index " <> show (i :: Int) <> " : " <> show r) (zip [0 ..] cronSchemas)
