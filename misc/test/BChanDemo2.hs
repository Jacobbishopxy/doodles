{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: BChanDemo2.hs
-- author: Jacob Xie
-- date: 2024/05/19 17:34:24 Sunday
-- brief:

import Brick
import Brick.BChan
import Brick.Widgets.Border (hBorder)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.IO
import System.Process

----------------------------------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------------------------------

-- Define custom event type
data CustomEvent where
  MessageEvent :: (Int, String) -> CustomEvent

-- Define the application state
data AppState = AppState
  { _eventChan :: BChan CustomEvent,
    _concurrentMsg :: [[String]]
  }

makeLenses ''AppState

-- The name for our UI elements
data Name = Name deriving (Ord, Show, Eq)

----------------------------------------------------------------------------------------------------
-- TUI
----------------------------------------------------------------------------------------------------

-- Brick app definition
app :: App AppState CustomEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appAttrMap = const theMap,
      appStartEvent = return ()
    }

-- Drawing the UI
drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = vBox [vBox (map (str . show) b) <=> hBorder | b <- _concurrentMsg s]

----------------------------------------------------------------------------------------------------

-- Event handling
handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (AppEvent (MessageEvent (idx, msg))) =
  modify $ appendMsg idx msg
-- add a new msg channel
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [])) =
  modify $ concurrentMsg %~ ([] :)
-- remove the last msg channel
handleEvent (VtyEvent (V.EvKey (V.KChar 'x') [])) =
  modify $ concurrentMsg %~ drop 1
-- send a msg to every msg channel
handleEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  st <- get
  let chan = st ^. eventChan
      bs = st ^. concurrentMsg
  forM_ (zip bs [0 ..]) $ \(_, i) ->
    liftIO $ void $ forkIO $ do
      writeBChan chan $ MessageEvent (i, "Hello BChan!")
      threadDelay 1_000_000 -- 1 second delay
handleEvent (VtyEvent (V.EvKey (V.KChar 'm') [])) = do
  st <- get
  let chan = st ^. eventChan
      bs = st ^. concurrentMsg

  forM_ (zip bs [0 ..]) $ \(_, i) -> do
    -- Start the subprocess
    (_, hOut, _, _) <-
      liftIO $
        createProcess
          (proc "/bin/bash" ["./scripts/rand_print.sh", "-h", "3", "-l", "1", "-m", "5"])
            { std_out = CreatePipe,
              std_err = CreatePipe
            }

    case hOut of
      Just o -> do
        void $ liftIO $ forkIO $ enqueueOutput i o chan
      _ -> return ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

-- Attribute map
theMap :: AttrMap
theMap = attrMap V.defAttr []

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

enqueueOutput :: Int -> Handle -> BChan CustomEvent -> IO ()
enqueueOutput idx hOut chan = do
  -- putStrLn "enqueueOutput..."
  ans <- try $ hGetLine hOut :: IO (Either IOError String)
  case ans of
    Left _ -> return ()
    Right line -> do
      writeBChan chan (MessageEvent (idx, line))
      enqueueOutput idx hOut chan -- Continue reading

appendMsg :: Int -> String -> AppState -> AppState
appendMsg idx m = (concurrentMsg . ix idx) %~ (m :)

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

-- Main function
main :: IO ()
main = do
  bchan <- newBChan 10
  let initialState = AppState bchan [[]]

  -- Run the Brick app
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just bchan) app initialState
