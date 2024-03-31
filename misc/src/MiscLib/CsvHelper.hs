{-# LANGUAGE OverloadedStrings #-}

-- file: CsvHelper.hs
-- author: Jacob Xie
-- date: 2024/03/31 16:31:52 Sunday
-- brief:

module MiscLib.CsvHelper
  ( readCsv,
    ParseRecord (..),
    readFloat,
    readInt,
    readInteger,
    readString,
    readBool,
  )
where

import Data.List (elemIndex)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readCsv :: (ParseRecord a) => FilePath -> IO (Either String [a])
readCsv file = do
  contents <- TIO.readFile file

  case T.lines contents of
    (headerLine : bodyLines) ->
      return $ Right [parseRecord (rowSplit headerLine) (rowSplit r) | r <- bodyLines]
    _ -> return $ Left "Insufficient lines in CSV file"
  where
    rowSplit :: T.Text -> [T.Text]
    rowSplit = T.splitOn "," . T.strip

class ParseRecord a where
  -- based on a header, turn a row into a record
  parseRecord :: [T.Text] -> [T.Text] -> a

----------------------------------------------------------------------------------------------------

getField :: (Eq a) => [a] -> [b] -> a -> b
getField headers row name = case name `elemIndex` headers of
  Just i -> row !! i
  Nothing -> error "Element not in list"

readFloat :: [T.Text] -> [T.Text] -> T.Text -> Float
readFloat header row name = read $ T.unpack (getField header row name)

readInt :: [T.Text] -> [T.Text] -> T.Text -> Int
readInt header row name = case reads $ T.unpack (getField header row name) of
  [(x, "")] -> x
  _ -> 0

readInteger :: [T.Text] -> [T.Text] -> T.Text -> Integer
readInteger header row name = read $ T.unpack (getField header row name)

readString :: [T.Text] -> [T.Text] -> T.Text -> String
readString header row name = T.unpack $ getField header row name

readBool :: [T.Text] -> [T.Text] -> T.Text -> Bool
readBool header row name = case getField header row name of
  "TRUE" -> True
  "true" -> True
  _ -> False
