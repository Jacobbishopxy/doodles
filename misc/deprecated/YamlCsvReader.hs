{-# LANGUAGE DeriveGeneric #-}

-- file: YamlCsvReader.hs
-- author: Jacob Xie
-- date: 2024/03/03 10:12:13 Sunday
-- brief:

module Main where

import Data.Aeson (FromJSON)
import Data.Either (fromRight)
import Data.Yaml qualified as Y
import GHC.Generics (Generic)
import MiscLib.CronSchema (getAllCrons)
import System.Environment (getArgs)

data CronSettings = CronSettings
  { lookupDirs :: [String],
    version :: Float
  }
  deriving (Show, Generic)

instance FromJSON CronSettings

main :: IO ()
main = do
  args <- getArgs
  let yamlPath = head args
  parsedContent <- Y.decodeFileEither yamlPath
  let cs = fromRight (error "check Yaml if exists") parsedContent
  crons <- getAllCrons $ lookupDirs cs
  mapM_ print crons
