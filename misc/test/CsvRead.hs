-- file: CsvRead.hs
-- author: Jacob Xie
-- date: 2024/03/29 21:11:32 Friday
-- brief:

module Main where

import Data.ByteString.Lazy qualified as BS
-- import Data.ByteString.Char8 qualified as C

import Data.Text qualified as T
import Data.Text.ICU.Convert qualified as ICU
import System.Environment (getArgs)
import System.IO qualified as SIO

parse :: T.Text -> [T.Text]
parse t = f t [] [] 0
  where
    f :: T.Text -> String -> [T.Text] -> Int -> [T.Text]
    f s cur line _ | T.length s == 1 = reverse (T.pack (reverse cur) : line)
    f s cur line n | T.head s == ',' && even n = f (T.tail s) [] (T.pack (reverse cur) : line) 0
    f s cur line n = f (T.tail s) (T.head s : cur) line (if T.head s == '"' then n + 1 else n)

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  -- Open the file in binary mode
  fileHandle <- SIO.openBinaryFile file SIO.ReadMode
  -- Read the file contents as ByteString
  fileBytes <- BS.hGetContents fileHandle

  -- Create a converter for converting from GBK to UTF-8
  converter <- ICU.open "utf-8" Nothing
  -- Convert the ByteString contents from GBK to UTF-8 encoded Text
  let decodedContents = ICU.toUnicode converter (BS.toStrict fileBytes)

  mapM_ print [parse x | x <- T.lines decodedContents]
