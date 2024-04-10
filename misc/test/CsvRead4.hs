-- file: CsvRead4.hs
-- author: Jacob Xie
-- date: 2024/04/10 17:28:15 Wednesday
-- brief:

module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Vector qualified as DV
import Data.Word
import HaskellWorks.Data.Dsv.Lazy.Cursor qualified as SVL
import HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy qualified as SVL
import System.Environment (getArgs)

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  bs <- LBS.readFile file
  let c = SVL.makeCursor (charToWord8 ',') bs
  let rows :: [DV.Vector LBS.ByteString] = SVL.toListVector c
  print rows

  return ()
