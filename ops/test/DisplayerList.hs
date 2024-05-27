{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: DisplayerList.hs
-- author: Jacob Xie
-- date: 2024/05/27 10:47:29 Monday
-- brief:

module Main where

import Brick
import Brick.BChan
import Brick.Widgets.Border (hBorder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.IO
import System.Process

----------------------------------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------------------------------

-- DMessage: Key -> displayer name; Value -> message string
data DisplayerEvent = DMessage (String, String)

data AppState = AppState
  { _eventChan :: BChan DisplayerEvent,
    _displayerMessages :: Map.Map String (Vec.Vector String)
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "whatever"
