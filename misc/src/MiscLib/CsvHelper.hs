{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- file: CsvHelper.hs
-- author: Jacob Xie
-- date: 2024/03/31 16:31:52 Sunday
-- brief:
-- ref: https://github.com/haskell-hvr/cassava/blob/3f792495c1256dc2e3a503b4c5c4518dfce9c5d8/src/Data/Csv/Conversion.hs#L1169

----------------------------------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------------------------------
-- based on a header, turn a row into a record

module MiscLib.CsvHelper
  ( readCsv,
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

import qualified Data.HashMap.Lazy as HM
import Data.List (elemIndex)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Failure f r = String -> f r

type Success a f r = a -> f r

newtype Parser a = Parser
  { unParser ::
      forall (f :: * -> *) (r :: *).
      Failure f r ->
      Success a f r ->
      f r
  }

findIdx :: (Eq a) => [a] -> a -> Maybe Int
findIdx headers name = name `elemIndex` headers

findVal :: Int -> [a] -> Maybe a
findVal idx row = row !? idx

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : xs) !? i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

class ParseRecord a where
  parseRecord :: [T.Text] -> [T.Text] -> a

class ParseConf a where
  -- use a header to generate `Parser`
  parseConf :: [T.Text] -> Parser a

data CronSchema = CronSchema
  { id' :: Float,
    dag :: String,
    name' :: String,
    sleeper :: String,
    input :: Maybe String,
    cmd :: String,
    output :: Maybe String,
    activate :: Bool,
    retries :: Maybe Int,
    ps :: String
  }
  deriving (Show)

instance ParseConf CronSchema where
  parseConf header = undefined

----------------------------------------------------------------------------------------------------
-- Public Fn
----------------------------------------------------------------------------------------------------

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

getField' :: (Eq a) => [a] -> [b] -> a -> Maybe b
getField' header row name =
  findIdx header name >>= flip findVal row
