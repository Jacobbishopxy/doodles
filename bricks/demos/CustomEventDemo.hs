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

{-
  使用自定义的事件类型
  ref: https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#L369-L436

  因为我们经常需要通过 Vty 的事件输入来与应用的指定事件进行交互，brick 嵌套了用户应用的事件进了
  `BrickEvent` 的流中，使得用户的事件处理器能接收到。这类事件的类型在 `BrickEvent n e` 以及
  `App s e n` 中为类型 `e`。

  注意：通常用户的应用在没有自定义事件类型时无需改动（例如，`App MyState e MyName` 或将其设置
  为 `App MyState () MyName`）。

  1. 首先是定义事件类型；
-}
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

-- 2. 其次，在 event 注册时添加 `CustomEvent`，函数体内进行模式匹配。
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
  -- 3. 接下来是实际 *生成* 用户自定义事件，并注入至 `brick` 的事件流中，使其可以被事件处理器接收。
  -- 为此我们需要创建一个 `BChan` 来接收用户的自定义事件。创建了该通道后提供给 `brick` 的就不能是
  -- `defaultMain` 而是 `customMain` 了（详见 `FormDemo.hs`）。
  --
  -- 一个 `BChan`（bounded channel），可以在尝试写入新项之前保留有限数量的项并保持阻塞。本例中的
  -- `newBChan` 保有 10 个项。它可以确保当程序不能迅速处理事件时，仍保留 10 项在队列中等待处理。
  -- 通常而言，在选择通道容量和设计事件生产者时，要考虑事件处理的性能，以便在通道满时它们可以阻塞。
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Counter
    threadDelay 1_000_000

  void $ customMainWithDefaultVty (Just chan) theApp initialState
