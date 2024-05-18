{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: BChanDemo.hs
-- author: Jacob Xie
-- date: 2024/05/18 21:57:23 Saturday
-- brief:

import Brick
import Brick.BChan
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

-- Define custom event type
data CustomEvent where
  MessageEvent :: String -> CustomEvent

-- Define the application state
data AppState = AppState
  { _messages :: [String],
    _eventChan :: BChan CustomEvent
  }

makeLenses ''AppState

-- The name for our UI elements
data Name = Name deriving (Ord, Show, Eq)

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
drawUI s = [vBox $ map (str . show) (_messages s)]

-- Event handling
handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (AppEvent (MessageEvent msg)) = messages %= (msg :)
handleEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  -- TODO: System.Process createProcess
  st <- get
  let chan = st ^. eventChan
  liftIO $ void $ forkIO $ do
    writeBChan chan $ MessageEvent "Hello from the dynamically created channel!"
    threadDelay 1000000 -- 1 second delay
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

-- Attribute map
theMap :: AttrMap
theMap = attrMap V.defAttr []

-- Main function
main :: IO ()
main = do
  chan <- newBChan 10
  let initialState = AppState [] chan

  -- Run the Brick app
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState
