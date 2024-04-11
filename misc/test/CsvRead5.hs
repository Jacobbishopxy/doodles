{-# LANGUAGE OverloadedStrings #-}

-- file: TestGPT.hs
-- author: Jacob Xie
-- date: 2024/04/03 00:13:28 Wednesday
-- brief:

module Main where

import Data.ByteString qualified as BL
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import GHC.Float (double2Float)
import HaskellWorks.Data.Dsv.Lazy.Cursor qualified as SVL
import HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy qualified as SVL
import System.Environment (getArgs)

type RawRow = V.Vector BSL.ByteString

type RawResult = V.Vector RawRow

----------------------------------------------------------------------------------------------------

-- Define a function to parse a CSV file into a vector of vectors of ByteString
readCsvRaw :: FilePath -> IO RawResult
readCsvRaw f = do
  bs <- BSL.readFile f
  let c = SVL.makeCursor (toEnum . fromEnum $ ',') bs
  return $ SVL.toVectorVector c

----------------------------------------------------------------------------------------------------

-- Define a typeclass for parsing a single record
class FromRecord a where
  parseRecord :: HM.HashMap BSL.ByteString Int -> RawRow -> Maybe a

-- Get indices for fields from the first record
getFieldIndices :: RawRow -> HM.HashMap BSL.ByteString Int
getFieldIndices = V.ifoldl' (\acc i bs -> HM.insert bs i acc) HM.empty

----------------------------------------------------------------------------------------------------

readCsv :: (FromRecord a) => FilePath -> IO (V.Vector a)
readCsv f = do
  raw <- readCsvRaw f
  let header = V.head raw
      fieldIndices = getFieldIndices header
      records = V.tail raw

  -- print fieldIndices
  -- print records

  return $ V.mapMaybe (parseRecord fieldIndices) records

----------------------------------------------------------------------------------------------------

class Parser a where
  parse :: BSL.ByteString -> a

instance Parser String where
  parse = T.unpack . TE.decodeUtf8Lenient . BL.toStrict

instance Parser (Maybe String) where
  parse s = case f s of
    "" -> Nothing
    s' -> Just s'
    where
      f = T.unpack . TE.decodeUtf8Lenient . BL.toStrict

instance Parser Int where
  parse s = case TR.decimal . TE.decodeUtf8Lenient $ BL.toStrict s of
    Right (i, r) | r == T.empty -> i
    _ -> 0

instance Parser (Maybe Int) where
  parse s = case TR.decimal . TE.decodeUtf8Lenient $ BL.toStrict s of
    Right (i, r) | r == T.empty -> Just i
    _ -> Nothing

instance Parser Float where
  parse s = case TR.double . TE.decodeUtf8Lenient $ BL.toStrict s of
    Right (i, r) | r == T.empty -> double2Float i
    _ -> 0

instance Parser (Maybe Float) where
  parse s = case TR.double . TE.decodeUtf8Lenient $ BL.toStrict s of
    Right (i, r) | r == T.empty -> Just $ double2Float i
    _ -> Nothing

instance Parser Double where
  parse s = case TR.double . TE.decodeUtf8Lenient $ BL.toStrict s of
    Right (i, r) | r == T.empty -> i
    _ -> 0

instance Parser (Maybe Double) where
  parse s = case TR.double . TE.decodeUtf8Lenient $ BL.toStrict s of
    Right (i, r) | r == T.empty -> Just i
    _ -> Nothing

instance Parser Bool where
  parse s = case T.toLower $ f s of
    "true" -> True
    _ -> False
    where
      f = TE.decodeUtf8Lenient . BL.toStrict

instance Parser (Maybe Bool) where
  parse s = case T.toLower $ f s of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
    where
      f = TE.decodeUtf8Lenient . BL.toStrict

----------------------------------------------------------------------------------------------------

(.:) :: (Parser a) => (HM.HashMap BSL.ByteString Int, BSL.ByteString) -> RawRow -> Maybe a
(.:) (fieldIndices, field) vec = do
  idx <- HM.lookup field fieldIndices
  return $ parse $ vec V.! idx

----------------------------------------------------------------------------------------------------

data CronSchema = CronSchema
  { dag :: String,
    name :: String,
    sleeper :: String,
    input :: String,
    cmd :: String,
    output :: String,
    activate :: Bool
  }
  deriving (Show)

-- Implement FromRecord instance for MyRecord
instance FromRecord CronSchema where
  parseRecord fieldIndices vec = do
    CronSchema
      <$> (fieldIndices, "dag") .: vec
      <*> (fieldIndices, "name") .: vec
      <*> (fieldIndices, "sleeper") .: vec
      <*> (fieldIndices, "input") .: vec
      <*> (fieldIndices, "cmd") .: vec
      <*> (fieldIndices, "output") .: vec
      <*> (fieldIndices, "activate") .: vec

main :: IO ()
main = do
  args <- getArgs
  let file = head args

  crons :: V.Vector CronSchema <- readCsv file

  print crons
