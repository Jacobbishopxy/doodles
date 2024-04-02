{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- file: CsvHelperPro.hs
-- author: Jacob Xie
-- date: 2024/04/02 10:02:23 Tuesday
-- brief:
-- ref: https://github.com/haskell-hvr/cassava/blob/3f792495c1256dc2e3a503b4c5c4518dfce9c5d8/src/Data/Csv/Conversion.hs#L1169

module CsvHelperPro
  ( Parser (..),
  )
where

import Data.Kind (Type)
import Data.List (elemIndex)
import qualified Data.Text as T

----------------------------------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------------------------------

type Failure f r = String -> f r

type Success a f r = a -> f r

newtype Parser a = Parser
  { unParser ::
      forall (f :: Type -> Type) (r :: Type).
      Failure f r ->
      Success a f r ->
      f r
  }

----------------------------------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------------------------------

class ParseConf a where
  -- use a header to generate `Parser`
  parseConf :: [T.Text] -> Parser a

----------------------------------------------------------------------------------------------------
-- Fn
----------------------------------------------------------------------------------------------------

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

getField :: (Eq a) => [a] -> [b] -> a -> Maybe b
getField header row name =
  findIdx header name >>= flip findVal row

-- lookup :: FromField a => NamedRecord -> T.Text -> Parser a

runParser :: Parser a -> Either String a
runParser p = unParser p left right
  where
    left !errMsg = Left errMsg
    right !x = Right x
