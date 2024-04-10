{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead3.hs
-- author: Jacob Xie
-- date: 2024/04/09 08:53:00 Tuesday
-- brief:

module Main where

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.ICU.Convert qualified as ICU
import Data.Vector qualified as V
import System.Environment (getArgs)
import System.IO qualified as SIO

-- Function to decode ByteString from GBK to UTF-8 Text
decodeToUtf8 :: BS.ByteString -> IO T.Text
decodeToUtf8 bytes = do
  -- converter <- ICU.open "GBK" (Just True)
  converter <- ICU.open "GBK" Nothing
  let utf8Text = ICU.toUnicode converter bytes
  -- ICU.close converter
  return utf8Text

main :: IO ()
main = do
  args <- getArgs
  let file = Prelude.head args

  -- Open the file in binary mode
  fileHandle <- SIO.openBinaryFile file SIO.ReadMode
  -- Read the file contents as ByteString
  fileBytes <- BS.hGetContents fileHandle

  -- Decode the file contents from GBK to UTF-8
  decodedText <- decodeToUtf8 fileBytes

  -- print decodedText

  -- Parse the CSV file using cassava
  let csvData = Csv.decode Csv.NoHeader $ BSL.fromStrict $ TE.encodeUtf8 decodedText :: Either String (V.Vector (V.Vector T.Text))

  -- Handle the parsed CSV data
  case csvData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right rows -> print rows

  -- Close the file handle
  SIO.hClose fileHandle
