-- file: CsvRead.hs
-- author: Jacob Xie
-- date: 2024/03/29 21:11:32 Friday
-- brief:

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import System.Environment (getArgs)
import System.IO

parse :: ByteString -> [ByteString]
parse s = parse' s [] [] 0

parse' :: ByteString -> String -> [ByteString] -> Int -> [ByteString]
parse' s cur line _ | C.length s == 1 = reverse (C.pack (reverse cur) : line)
parse' s cur line n | C.head s == ',' && even n = parse' (C.tail s) [] (C.pack (reverse cur) : line) 0
parse' s cur line n = parse' (C.tail s) (C.head s : cur) line (if C.head s == '"' then n + 1 else n)

main :: IO ()
main = do
  args <- getArgs
  let fpath = head args

  handle <- openFile fpath ReadMode
  contents <- C.hGetContents handle
  mapM_ print [parse x | x <- C.lines contents]
