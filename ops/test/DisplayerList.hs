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
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
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
-- note, this can also be an Enum representing different kind of events
data DisplayerEvent = DMessage (Int, String)

-- type synonym: displayerMessages
type DMsg = [RingBuffer String]

-- type synonym: outputDisplay
type DOut = [List Name String]

data AppState = AppState
  { -- custom event channel
    _eventChan :: BChan DisplayerEvent,
    -- max displayer number on screen
    _maxDisplayerNum :: Int,
    -- caching messages
    _displayerMessages :: DMsg,
    -- brick focusRing
    _focusRing :: F.FocusRing Name,
    -- command input region
    _inputCmd :: Editor String Name,
    -- outputs on screen
    _outputDisplay :: DOut
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

singleDisplayer :: Bool -> (Int, List Name String) -> Widget Name
singleDisplayer onFocus (n, ms) =
  onF $
    borderWithLabel
      (withAttr borderTitleAttr $ str $ "Channel: " <> show n)
      (renderList ls False ms)
  where
    onF = if onFocus then updateAttrMap $ applyAttrMappings borderOnFocusedAttr else id
    ls :: Bool -> String -> Widget Name
    ls sel s =
      let ws = if sel then withAttr resultSelectedListAttr . str else str
       in ws s

displayerPanel :: AppState -> Widget Name
displayerPanel s = vBox [singleDisplayer (focusOn (fst p)) p | p <- zip [0 ..] (s ^. outputDisplay)]
  where
    focusOn i =
      case F.focusGetCurrent $ s ^. focusRing of
        Just (DisplayerRegion i') -> if i == i' then True else False
        _ -> False

----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name DisplayerEvent -> EventM Name AppState ()
-- Esc
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
-- BChan custom event
handleEvent (AppEvent d) = modify $ customEventHandling d
-- add one sub-displayer
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = do
  l <- use displayerMessages
  m <- use maxDisplayerNum
  focusRing %= F.focusSetCurrent CmdRegion
  if length l == m
    then return ()
    else
      (modify $ displayerMessages %~ expandDMsg)
        >> (modify $ outputDisplay %~ expandODip)

-- remove the last sub-displayer
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = do
  l <- use displayerMessages
  focusRing %= F.focusSetCurrent CmdRegion
  if length l == 1
    then return ()
    else
      (modify $ displayerMessages %~ recedeDMsg)
        >> (modify $ outputDisplay %~ recedeODip)

-- move down to the next displayer
handleEvent (VtyEvent (V.EvKey (V.KDown) [V.MCtrl])) = do
  r <- use focusRing
  m <- use displayerMessages
  case F.focusGetCurrent r of
    Just n -> focusRing %= F.focusSetCurrent (focusRingSwitching (length m) n)
    _ -> return ()

-- move up to the previous displayer
handleEvent (VtyEvent (V.EvKey (V.KUp) [V.MCtrl])) = do
  r <- use focusRing
  m <- use displayerMessages
  case F.focusGetCurrent r of
    Just n -> focusRing %= F.focusSetCurrent (focusRingSwitching' (length m) n)
    _ -> return ()

-- handle editor event & list event in each displayer
handleEvent ev@(VtyEvent ve) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just CmdRegion ->
      case ve of
        -- enter
        (V.EvKey V.KEnter []) -> do
          st <- get
          let chan = st ^. eventChan
              dm = st ^. displayerMessages
              msg = concat $ getEditContents $ st ^. inputCmd

          forM_ (zip [0 ..] dm) $ \(i, _) -> do
            -- start the subprocess
            (_, hOut, _, _) <-
              liftIO $
                createProcess
                  (proc "/bin/bash" ["./scripts/rand_print.sh", "-h", "3", "-l", "1", "-m", "5"])
                    { std_out = CreatePipe,
                      std_err = CreatePipe
                    }
            case hOut of
              Just o -> void $ liftIO $ forkIO $ sendingRandPrint (DMessage (i, msg)) o chan
              _ -> return ()
        _ -> zoom inputCmd $ handleEditorEvent ev
    Just (DisplayerRegion i) -> zoom (outputDisplay . ix i) $ handleListEvent ve
    _ -> return ()

-- do nothing
handleEvent _ = return ()

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

defaultRingBufferSize :: Int
defaultRingBufferSize = 100

initDMsg :: Int -> DMsg
initDMsg n = [newRingBuffer defaultRingBufferSize | _ <- [0 .. n - 1]]

initODip :: Int -> DOut
initODip n = [l i | i <- [0 .. n - 1]]
  where
    l i' = list (DisplayerRegion i') Vec.empty 1

initFRing :: Int -> [Name]
initFRing n = CmdRegion : [DisplayerRegion i | i <- [0 .. n]]

expandDMsg :: DMsg -> DMsg
expandDMsg = (++ [newRingBuffer defaultRingBufferSize])

recedeDMsg :: DMsg -> DMsg
recedeDMsg = init

expandODip :: DOut -> DOut
expandODip l = l ++ [list (DisplayerRegion $ length l + 1) Vec.empty 1]

recedeODip :: DOut -> DOut
recedeODip = init

-- n: number of displayer
initState :: BChan DisplayerEvent -> Int -> AppState
initState bc n =
  AppState
    { _eventChan = bc,
      _maxDisplayerNum = 8,
      _displayerMessages = initDMsg n,
      -- brick focusRing
      _focusRing = F.focusRing $ initFRing n,
      -- command input region
      _inputCmd = editor CmdRegion (Just 1) "",
      -- outputs on screen
      _outputDisplay = initODip n
    }

----------------------------------------------------------------------------------------------------

-- Attribute map
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.black),
      (editFocusedAttr, V.black `on` V.yellow),
      (listAttr, V.white `on` V.black),
      (listSelectedAttr, V.black `on` V.yellow),
      -- overwrite
      (resultSelectedListAttr, V.black `on` V.yellow),
      (resultUnselectedListAttr, V.white `on` V.black)
    ]

borderTitleAttr :: AttrName
borderTitleAttr = attrName "borderTitle"

borderOnFocusedAttr :: [(AttrName, V.Attr)]
borderOnFocusedAttr =
  [ (borderAttr, fg V.yellow),
    (borderTitleAttr, fg V.yellow)
  ]

resultSelectedListAttr :: AttrName
resultSelectedListAttr = listSelectedAttr <> attrName "resultSelectedList"

resultUnselectedListAttr :: AttrName
resultUnselectedListAttr = listAttr <> listSelectedAttr <> attrName "resultUnselectedList"

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

-- move downward
focusRingSwitching :: Int -> Name -> Name
focusRingSwitching n r =
  case r of
    CmdRegion -> DisplayerRegion 0
    (DisplayerRegion i) -> if i == n - 1 then CmdRegion else DisplayerRegion (i + 1)

-- move upward
focusRingSwitching' :: Int -> Name -> Name
focusRingSwitching' n r =
  case r of
    CmdRegion -> DisplayerRegion (n - 1)
    (DisplayerRegion i) -> if i == 0 then CmdRegion else DisplayerRegion (i - 1)

-- keyboard key-in message + bash stdout message, sending to `BChan`
sendingRandPrint :: DisplayerEvent -> Handle -> BChan DisplayerEvent -> IO ()
sendingRandPrint de hOut chan = do
  ans <- try $ hGetLine hOut :: IO (Either IOError String)
  case ans of
    Left _ -> return ()
    Right line -> writeBChan chan $ att de line
  where
    att :: DisplayerEvent -> String -> DisplayerEvent
    att (DMessage d) s = DMessage (fst d, snd d <> ": " <> s)

-- receiving custom events and modifying the state
-- TODO: modify `outputDisplay` as well
customEventHandling :: DisplayerEvent -> AppState -> AppState
customEventHandling (DMessage d) =
  displayerMessages . ix (fst d) %~ \rb -> appendRingBuffer rb (snd d)

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
