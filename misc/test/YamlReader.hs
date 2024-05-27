{-# LANGUAGE DeriveGeneric #-}

-- file: YamlReader.hs
-- author: Jacob Xie
-- date: 2024/03/03 10:30:45 Sunday
-- brief:

module Main where

import Data.Aeson
import Data.Yaml qualified as Y
import GHC.Generics
import System.Environment (getArgs)

data CronSettings = CronSettings
  { lookupDirs :: [String],
    version :: Float
  }
  deriving (Show, Generic)

instance FromJSON CronSettings

main :: IO ()
main = do
  (fp : _) <- getArgs
  parsedContent <- Y.decodeFileEither fp :: IO (Either Y.ParseException CronSettings)
  print parsedContent
