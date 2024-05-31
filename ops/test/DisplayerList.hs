{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: DisplayerList.hs
-- author: Jacob Xie
-- date: 2024/05/27 10:47:29 Monday
-- brief:

module Main where

import Brick
import Brick.BChan
import qualified Brick.Focus as F
import Brick.Forms
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Edit
import Brick.Widgets.List
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
import OpsLib.RingBuffer
import System.IO
import System.Process

{-
  The purpose of this test script is to explore how to dynamically create/remove sub-displayers,
  and to use the concurrent lib to execute one command in each sub-displayers (simulating SSH to
  different IP address).
-}

----------------------------------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------------------------------

-- UI elements
data Name = CmdRegion | DisplayerRegion Int deriving (Ord, Show, Eq)

-- DMessage: Key -> displayer index; Value -> message string
data DisplayerEvent = DMessage (Int, String)

data AppState = AppState
  { -- custom event channel
    _eventChan :: BChan DisplayerEvent,
    -- max displayer number on screen
    _maxDisplayerNum :: Int,
    -- caching messages
    _displayerMessages :: Map.Map Int (RingBuffer String),
    -- brick focusRing
    _focusRing :: F.FocusRing Name,
    -- command input region
    _inputCmd :: Editor String Name,
    -- outputs on screen
    _outputDip :: Map.Map Int (List Name String)
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- TUI
----------------------------------------------------------------------------------------------------

-- UI
drawUI :: AppState -> [Widget Name]
drawUI s = [vBox [cmdPanel s <=> displayerPanel s]]

cmdPanel :: AppState -> Widget Name
cmdPanel s =
  borderWithLabel (str "cmd") $
    F.withFocusRing
      (s ^. focusRing)
      (renderEditor $ str . unlines)
      (s ^. inputCmd)

singleDisplayer :: (Int, List Name String) -> Widget Name
singleDisplayer (n, ms) =
  borderWithLabel
    (str $ "Channel: " <> show n)
    (renderList ls False ms)
  where
    ls :: Bool -> String -> Widget Name
    ls sel s =
      let ws = if sel then withAttr resultSelectedListAttr . str else str
       in ws s

displayerPanel :: AppState -> Widget Name
displayerPanel s = vBox [singleDisplayer p | p <- Map.toList $ s ^. outputDip]

----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name DisplayerEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

----------------------------------------------------------------------------------------------------

-- Attribute map
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.black),
      (editFocusedAttr, V.black `on` V.yellow),
      (listAttr, V.white `Brick.on` V.black),
      (listSelectedAttr, V.black `Brick.on` V.yellow),
      (formAttr, V.white `Brick.on` V.black),
      (focusedFormInputAttr, V.black `on` V.yellow),
      -- overwrite
      (invisibleFormFieldAttr, fg V.black),
      (resultHeaderListAttr, V.white `on` V.blue),
      (resultSelectedListAttr, V.black `on` V.yellow),
      (resultUnselectedListAttr, V.white `on` V.black),
      (detailSelectedListAttr, V.white `on` V.black)
    ]

invisibleFormFieldAttr :: AttrName
invisibleFormFieldAttr = focusedFormInputAttr <> attrName "invisibleFormField"

resultHeaderListAttr :: AttrName
resultHeaderListAttr = listAttr <> listSelectedAttr <> attrName "resultHeaderList"

resultSelectedListAttr :: AttrName
resultSelectedListAttr = listSelectedAttr <> attrName "resultSelectedList"

resultUnselectedListAttr :: AttrName
resultUnselectedListAttr = listAttr <> listSelectedAttr <> attrName "resultUnselectedList"

detailSelectedListAttr :: AttrName
detailSelectedListAttr = listSelectedAttr <> attrName "detailSelectedList"

----------------------------------------------------------------------------------------------------

app :: App AppState DisplayerEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appAttrMap = const theMap,
      appStartEvent = return ()
    }

initDMsg :: Int -> Map.Map Int (RingBuffer String)
initDMsg n = Map.fromList [(i, newRingBuffer 100) | i <- [0 .. n - 1]]

initDMsg' :: Int -> Map.Map Int (List Name String)
initDMsg' n = Map.fromList [(i, l i) | i <- [0 .. n - 1]]
  where
    l i' = list (DisplayerRegion i') Vec.empty 1

-- n: number of displayer
initState :: BChan DisplayerEvent -> Int -> AppState
initState bc n =
  AppState
    { _eventChan = bc,
      _maxDisplayerNum = 8,
      _displayerMessages = initDMsg n,
      -- brick focusRing
      _focusRing = F.focusRing [CmdRegion, DisplayerRegion 0],
      -- command input region
      _inputCmd = editor CmdRegion (Just 1) "",
      -- outputs on screen
      _outputDip = initDMsg' n
    }

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  bchan <- newBChan 100

  -- Run the Brick app
  let buildVty = mkVty V.defaultConfig
      initialState = initState bchan 4

  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just bchan) app initialState
