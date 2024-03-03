-- file: DotenvReader.hs
-- author: Jacob Xie
-- date: 2024/03/03 15:26:02 Sunday
-- brief:

module MiscLib.DotenvReader
  ( readEnvFile,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- read a .env file and parse key-value pairs
readEnvFile :: FilePath -> IO (M.Map String String)
readEnvFile f = do
  ctt <- readFileLines f
  return $ parseEnvFile ctt

-- read file contents line by line
readFileLines :: FilePath -> IO [String]
readFileLines f = do
  h <- openFile f ReadMode
  ctt <- hGetContents h
  return $ lines ctt

-- parse .env file contents and extract key-value pairs
parseEnvFile :: [String] -> M.Map String String
parseEnvFile [] = M.empty
parseEnvFile (l : rest) =
  case splitOn "=" l of
    [k, v] -> M.insert k v $ parseEnvFile rest
    _ -> parseEnvFile rest
