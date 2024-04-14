{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: CsvRead7Util.hs
-- author: Jacob Xie
-- date: 2024/04/14 09:39:31 Sunday
-- brief:

module CsvRead7Util
  ( module CsvRead7Util,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 (string)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.Functor
import Data.HashMap.Strict qualified as HM
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8)

-- | CSV data represented as a Haskell vector of vector of
-- bytestrings.
type Csv = Vector Record

-- | A record corresponds to a single line in a CSV file.
type Record = Vector Field

-- | The header corresponds to the first line a CSV file. Not all CSV
-- files have a header.
type Header = Vector Name

-- | A header has one or more names, describing the data in the column
-- following the name.
type Name = B.ByteString

-- | A record corresponds to a single line in a CSV file, indexed by
-- the column name rather than the column index.
type NamedRecord = HM.HashMap B.ByteString B.ByteString

-- | A single field within a record.
type Field = B.ByteString

----------------------------------------------------------------------------------------------------

toStrict :: L.ByteString -> B.ByteString
toStrict = B.concat . L.toChunks

-- | Convert a 'Record' to a 'NamedRecord' by attaching column names.
-- The 'Header' and 'Record' must be of the same length.
toNamedRecord :: Header -> Record -> NamedRecord
toNamedRecord hdr v = HM.fromList . V.toList $ V.zip hdr v

-- | Is the CSV data preceded by a header?
data HasHeader
  = -- | The CSV data is preceded by a header
    HasHeader
  | -- | The CSV data is not preceded by a header
    NoHeader

-- | A strict version of 'Data.Functor.<$>' for monads.
(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
f <$!> m = do
  a <- m
  return $! f a
{-# INLINE (<$!>) #-}

infixl 4 <$!>

-- | Is this an empty record (i.e. a blank line)?
blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && B.null (V.head v)

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  f x <$> b
{-# INLINE liftM2' #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@, or a single
-- carriage return @\'\\r\'@.
endOfLine :: Parser ()
endOfLine = (A.word8 newline $> ()) <|> (string "\r\n" $> ()) <|> (A.word8 cr $> ())
{-# INLINE endOfLine #-}

doubleQuote, newline, cr :: Word8
doubleQuote = 34
newline = 10
cr = 13
