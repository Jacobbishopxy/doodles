-- file: CronSearchUtil.hs
-- author: Jacob Xie
-- date: 2024/04/06 22:35:02 Saturday
-- brief:

module CronSearchUtil
  ( CronSettings (..),
    resultBoxColumns,
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

----------------------------------------------------------------------------------------------------
-- Const
----------------------------------------------------------------------------------------------------

resultBoxColumns :: [String]
resultBoxColumns = ["idx", "dag", "name", "sleeper", "input", "cmd", "output", "activate", "fPath"]

----------------------------------------------------------------------------------------------------
-- Conf
----------------------------------------------------------------------------------------------------

data CronSettings where
  CronSettings :: {lookupDirs :: [String], version :: Float} -> CronSettings
  deriving (Show, Generic)

instance FromJSON CronSettings