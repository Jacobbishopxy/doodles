{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead2.hs
-- author: Jacob Xie
-- date: 2024/04/08 15:00:04 Monday
-- brief:

module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Vector qualified as V
import System.Environment (getArgs)
import System.IO qualified as SIO

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  -- Open the file with GBK encoding explicitly
  fileHandle <- SIO.openFile file SIO.ReadMode
  -- Read the file contents as ByteString
  fileBytes <- BS.hGetContents fileHandle

  -- Decode the file contents from GBK to UTF-8
  let decodedText = TE.decodeUtf8With TE.lenientDecode fileBytes

  -- Parse the CSV file using cassava
  let csvData = Csv.decode Csv.NoHeader (BSL.fromStrict $ TE.encodeUtf8 decodedText) :: Either String (V.Vector (V.Vector T.Text))

  -- Handle the parsed CSV data
  case csvData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right rows -> do
      let listOfLists = map V.toList (V.toList rows)
      putStrLn "Contents of the CSV file:"
      mapM_ print listOfLists

  -- Close the file handle
  SIO.hClose fileHandle
