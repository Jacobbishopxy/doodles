{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: ShowCsv.hs
-- author: Jacob Xie
-- date: 2024/03/12 12:20:06 Tuesday
-- brief:

module Main where

-- import Brick
-- import Brick.BChan
-- import Control.Concurrent
-- import Control.Monad (void)
-- import Data.Text qualified as T
-- import Graphics.Vty qualified as V
-- import Lens.Micro.Mtl
-- import Lens.Micro.TH

import Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as Vec
import System.Environment (getArgs)

type CsvResult = Vec.Vector T.Text

readCsv :: FilePath -> IO (Either String CsvResult)
readCsv file = do
  contents <- BL.readFile file
  return $ Vec.map cvtRow <$> decode HasHeader contents

-- convert vector of ByteString into Text
cvtRow :: Vec.Vector BL.ByteString -> T.Text
cvtRow = T.intercalate "," . Prelude.map (TE.decodeUtf8 . BL.toStrict) . Vec.toList

main :: IO ()
main = do
  args <- getArgs
  let csvFile = Prelude.head args
  result <- readCsv csvFile
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right rawStrings -> do
      putStrLn "Raw strings from CSV file:"
      print rawStrings
