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
import MiscLib.CronSchema (searchAllCrons)
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
  parsedContent <- Y.decodeFileEither (head args) :: IO (Either Y.ParseException CronSettings)
  let cs = fromRight (error "check Yaml") parsedContent
  crons <- searchAllCrons $ lookupDirs cs
  mapM_ print crons
