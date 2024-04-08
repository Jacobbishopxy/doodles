{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead3.hs
-- author: Jacob Xie
-- date: 2024/04/08 15:00:04 Monday
-- brief:

module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.ICU.Convert qualified as ICU
import Data.Vector qualified as V
import System.Environment (getArgs)
import System.IO qualified as SIO

-- Function to decode ByteString from GBK to UTF-8 Text
decodeToUtf8 :: BSL.ByteString -> IO T.Text
decodeToUtf8 bytes = do
  converter <- ICU.open "GBK" (Just True)
  let utf8Text = ICU.toUnicode converter (BS.toStrict bytes)
  return utf8Text

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  -- Open the file with GBK encoding explicitly
  fileHandle <- SIO.openFile file SIO.ReadMode
  -- Read the file contents as ByteString
  fileBytes <- BSL.hGetContents fileHandle

  -- Decode the file contents from GBK to UTF-8
  decodedText <- decodeToUtf8 fileBytes

  let d = BSL.fromStrict $ TE.encodeUtf8 decodedText

  -- Parse the CSV file using cassava
  let csvData = Csv.decode Csv.NoHeader d :: Either String (V.Vector (V.Vector T.Text))

  -- Handle the parsed CSV data
  case csvData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right rows -> do
      let listOfLists = map V.toList (V.toList rows)
      putStrLn "Contents of the CSV file:"
      mapM_ print listOfLists

  -- Close the file handle
  SIO.hClose fileHandle
