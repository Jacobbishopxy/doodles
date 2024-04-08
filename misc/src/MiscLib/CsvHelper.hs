{-# LANGUAGE OverloadedStrings #-}

-- file: CsvHelper.hs
-- author: Jacob Xie
-- date: 2024/03/31 16:31:52 Sunday
-- brief:

module MiscLib.CsvHelper
  ( readCsv,
    readCsv',
    ParseRecord (..),
    readFloat,
    readFloat',
    readInt,
    readInt',
    readInteger,
    readInteger',
    readString,
    readString',
    readBool,
    readBool',
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import Data.List (elemIndex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as Vec
import qualified System.IO as SIO

----------------------------------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------------------------------

-- based on a header, turn a row into a record
class ParseRecord a where
  parseRecord :: [T.Text] -> [T.Text] -> a

----------------------------------------------------------------------------------------------------
-- Public Fn
----------------------------------------------------------------------------------------------------

readCsv' :: (Csv.FromNamedRecord a) => FilePath -> IO (Either String (Csv.Header, Vec.Vector a))
readCsv' file = do
  h <- SIO.openFile file SIO.ReadMode
  b <- BS.hGetContents h

  let decodedText = TE.decodeUtf8With TE.lenientDecode b

  let csvData = Csv.decodeByName (BSL.fromStrict $ TE.encodeUtf8 decodedText)

  return csvData

readCsv :: (ParseRecord a) => FilePath -> IO (Either String [a])
readCsv file = do
  contents <- TIO.readFile file

  case T.lines contents of
    (headerLine : bodyLines) ->
      return $ Right [parseRecord (rowSplit headerLine) (rowSplit r) | r <- bodyLines]
    _ -> return $ Left "Insufficient lines in CSV file"

readFloat :: [T.Text] -> [T.Text] -> T.Text -> Float
readFloat header row name = case reads $ T.unpack t of
  [(x, "")] -> x
  _ -> error $ "readFloat failed on: " <> show t
  where
    t = getField header row name

readFloat' :: [T.Text] -> [T.Text] -> T.Text -> Maybe Float
readFloat' header row name = case reads $ T.unpack t of
  [(x, "")] -> Just x
  _ -> Nothing
  where
    t = getField header row name

readInt :: [T.Text] -> [T.Text] -> T.Text -> Int
readInt header row name = case reads $ T.unpack t of
  [(x, "")] -> x
  _ -> error $ "readInt failed on: " <> show t
  where
    t = getField header row name

readInt' :: [T.Text] -> [T.Text] -> T.Text -> Maybe Int
readInt' header row name = case reads $ T.unpack t of
  [(x, "")] -> Just x
  _ -> Nothing
  where
    t = getField header row name

readInteger :: [T.Text] -> [T.Text] -> T.Text -> Integer
readInteger header row name = case reads $ T.unpack t of
  [(x, "")] -> x
  _ -> error $ "readInteger failed on: " <> show t
  where
    t = getField header row name

readInteger' :: [T.Text] -> [T.Text] -> T.Text -> Maybe Integer
readInteger' header row name = case reads $ T.unpack t of
  [(x, "")] -> Just x
  _ -> Nothing
  where
    t = getField header row name

readString :: [T.Text] -> [T.Text] -> T.Text -> String
readString header row name = T.unpack $ getField header row name

readString' :: [T.Text] -> [T.Text] -> T.Text -> Maybe String
readString' header row name = case T.unpack $ getField header row name of
  e | e == "" -> Nothing
  s -> Just s

readBool :: [T.Text] -> [T.Text] -> T.Text -> Bool
readBool header row name = case getField header row name of
  "TRUE" -> True
  "true" -> True
  _ -> False

readBool' :: [T.Text] -> [T.Text] -> T.Text -> Maybe Bool
readBool' header row name = case getField header row name of
  "TRUE" -> Just True
  "true" -> Just True
  "FALSE" -> Just False
  "false" -> Just False
  _ -> Nothing

----------------------------------------------------------------------------------------------------
-- Private Fn
----------------------------------------------------------------------------------------------------

rowSplit :: T.Text -> [T.Text]
rowSplit t = f t [] [] 0
  where
    f :: T.Text -> String -> [T.Text] -> Int -> [T.Text]
    f s cur line _ | T.length s == 1 = reverse (T.pack (reverse cur) : line)
    f s cur line n | T.head s == ',' && even n = f (T.tail s) [] (T.pack (reverse cur) : line) 0
    f s cur line n = f (T.tail s) (T.head s : cur) line (if T.head s == '"' then n + 1 else n)

getField :: (Eq a) => [a] -> [b] -> a -> b
getField header row name = case name `elemIndex` header of
  Just i -> row !! i
  Nothing -> error "Element not in list"
