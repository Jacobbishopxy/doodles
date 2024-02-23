{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: CustomEventDemo.hs
-- author: Jacob Xie
-- date: 2024/02/23 10:02:59 Friday
-- brief: count & print keyboard event

module Main where

import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App (..), customMainWithDefaultVty, halt, showFirstCursor)
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Widgets.Core (str, (<=>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((%=), (.=))
import Lens.Micro.TH (makeLenses)

data CustomEvent = Counter deriving (Show)

data St = St
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int
  }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [a]
  where
    a =
      str ("Last event: " <> show (st ^. stLastBrickEvent))
        <=> str ("Counter value is: " <> show (st ^. stCounter))

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent _ -> stLastBrickEvent .= Just e
    AppEvent Counter -> do
      stCounter %= (+ 1)
      stLastBrickEvent .= Just e
    _ -> return ()

initialState :: St
initialState = St {_stLastBrickEvent = Nothing, _stCounter = 0}

theApp :: App St CustomEvent ()
theApp =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr []
    }

main :: IO ()
main = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Counter
    threadDelay 1_000_000

  void $ customMainWithDefaultVty (Just chan) theApp initialState
