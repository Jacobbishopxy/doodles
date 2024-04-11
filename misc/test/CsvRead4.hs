-- file: CsvRead4.hs
-- author: Jacob Xie
-- date: 2024/04/10 17:28:15 Wednesday
-- brief:

module Main where

import Data.ByteString.Lazy qualified as BSL
import Data.List (nub)
import Data.Vector qualified as V
import Data.Word
import HaskellWorks.Data.Dsv.Lazy.Cursor qualified as SVL
import HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy qualified as SVL
import System.Environment (getArgs)

type RawResult = V.Vector (V.Vector BSL.ByteString)

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

readCsvRaw :: FilePath -> IO RawResult
readCsvRaw f = do
  bs <- BSL.readFile f
  let c = SVL.makeCursor (charToWord8 ',') bs
  return $ SVL.toVectorVector c

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  rows <- readCsvRaw file
  print rows

  let r = nub $ V.toList $ V.map V.length rows
  print r

  return ()
