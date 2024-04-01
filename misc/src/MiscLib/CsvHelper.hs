{-# LANGUAGE OverloadedStrings #-}

-- file: CsvHelper.hs
-- author: Jacob Xie
-- date: 2024/03/31 16:31:52 Sunday
-- brief:

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

import Data.List (elemIndex)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

----------------------------------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------------------------------

class ParseRecord a where
  -- based on a header, turn a row into a record
  parseRecord :: [T.Text] -> [T.Text] -> a

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
getField headers row name = case name `elemIndex` headers of
  Just i -> row !! i
  Nothing -> error "Element not in list"
