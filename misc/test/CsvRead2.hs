{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead2.hs
-- author: Jacob Xie
-- date: 2024/04/08 15:00:04 Monday
-- brief:

module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import System.Environment (getArgs)
import System.IO qualified as SIO

type CsvResult a = Either String (Header, V.Vector a)

type CsvResult' = Either String (V.Vector (V.Vector T.Text))

readCsv :: (FromNamedRecord a) => FilePath -> IO (CsvResult a)
readCsv file = do
  h <- SIO.openFile file SIO.ReadMode
  b <- BS.hGetContents h

  let decodedText = TE.decodeUtf8Lenient b

  return $ decodeByName $ BSL.fromStrict $ TE.encodeUtf8 decodedText

readCsv' :: FilePath -> IO CsvResult'
readCsv' file = do
  h <- SIO.openFile file SIO.ReadMode
  fileBytes <- BS.hGetContents h

  -- Decode the file contents from GBK to UTF-8
  let decodedText = TE.decodeUtf8Lenient fileBytes

  -- Parse the CSV file using cassava
  return $ decode NoHeader $ BSL.fromStrict $ TE.encodeUtf8 decodedText

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: String
  }
  deriving (Show)

instance FromNamedRecord CronSchema where
  parseNamedRecord m =
    CronSchema
      <$> m .: "dag"
      <*> m .: "name"
      <*> m .: "sleeper"
      <*> m .: "input"
      <*> m .: "cmd"
      <*> m .: "output"
      <*> m .: "activate"

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  csvData :: CsvResult CronSchema <- readCsv file
  case csvData of
    Left err -> error err
    Right d -> do
      V.mapM_ print $ snd d

-- csvData' :: CsvResult' <- readCsv' file
-- case csvData' of
--   Left err -> error err
--   Right d -> V.mapM_ print d
