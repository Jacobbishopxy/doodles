{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead6.hs
-- author: Jacob Xie
-- date: 2024/04/13 22:23:45 Saturday
-- brief:

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.ByteString qualified as BS
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import System.Environment (getArgs)
import System.IO
import Prelude hiding (concat, takeWhile)

type CSV = [[T.Text]]

lineEnd :: Parser ()
lineEnd =
  void (char '\n')
    <|> void (string "\r\n")
    <|> void (char '\r')
    <?> "end of line"

unquotedField :: Parser T.Text
unquotedField =
  takeWhile (\c -> c /= ',' && c /= '\n' && c /= '\r' && c /= '"')
    <?> "unquoted field"

insideQuotes :: Parser T.Text
insideQuotes =
  T.append
    <$> takeWhile (/= '"')
    <*> (T.concat <$> many (T.cons <$> dquotes <*> insideQuotes))
    <?> "inside of double quotes"
  where
    dquotes =
      string "\"\""
        >> return '"'
        <?> "paired double quotes"

quotedField :: Parser T.Text
quotedField =
  char '"'
    *> insideQuotes
    <* char '"'
    <?> "quoted field"

field :: Parser T.Text
field =
  quotedField
    <|> unquotedField
    <?> "field"

record :: Parser [T.Text]
record =
  field
    `sepBy1` char ','
    <?> "record"

file :: Parser CSV
file =
  (:)
    <$> record
    <*> manyTill
      (lineEnd *> record)
      (endOfInput <|> lineEnd *> endOfInput)
    <?> "file"

parseCSV :: T.Text -> Either String CSV
parseCSV = parseOnly file

readCsv :: FilePath -> IO (Either String CSV)
readCsv fp = do
  h <- openFile fp ReadMode
  b <- BS.hGetContents h

  let decodedText = TE.decodeUtf8With TE.lenientDecode b
  -- print decodedText

  return $ parseCSV decodedText

-- Example usage
main :: IO ()
main = do
  args <- getArgs
  let f = head args

  result <- readCsv f
  case result of
    Left err -> putStrLn $ "Error parsing CSV: " ++ err
    Right csv -> do
      mapM_
        (\(idx, r) -> putStrLn $ "idx: " <> show (idx :: Int) <> ", len: " <> show (length r) <> ", row: " <> show r)
        (zip [0 ..] csv)
