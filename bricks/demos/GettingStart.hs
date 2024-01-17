-- file: GettingStart.hs
-- author: Jacob Xie
-- date: 2024/01/16 09:27:23 Tuesday
-- brief:

module Main where

import Brick
import Brick.Widgets.Border
import Graphics.Vty.Attributes

type AppState = Int

{-
  `Brick.Main.App` 类型是一个 record 类型，有三个类型参数构成（`data App s e n`）：

  - application state type，应用的状态类型
  - event type，事件类型
  - resource name type，资源名称类型

  类型签名：
  data App s e n =
    App { appDraw         :: s -> [Widget n]
        , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
        , appHandleEvent  :: BrickEvent n e -> EventM n s ()
        , appStartEvent   :: EventM n s ()
        , appAttrMap      :: s -> AttrMap
        }
-}
app :: App AppState () String
app =
  App
    { -- `appDraw` 是一个函数用于转换当前应用状态成为一个 `Widget` 类型的列表，从上至下构成图形截面。
      -- 每个 `Widget` 会转换成一个 `vty` 层，再打印至终端。
      -- `Widget` 类型是绘图指示。绘图函数中会构建或转换若干 `Widget` 至用于描绘界面。这些指示会根据以下三种因素被执行：
      -- 1. 终端的大小
      -- 2. 应用的属性映射（`appAttrMap`）
      -- 3. 可滚动的可视状态
      --
      -- `appDraw` 函数在事件循环（event loop）开始时被调用。同样也会在一个事件被 `appHandleEvent` 处理后被调用。
      appDraw = appDraw',
      -- `appHandleEvent` 根据新的事件来决定如何更新状态。
      -- 入参 `BrickEvent` 为需要处理的事件，其自身的参数 `n` 与 `e` 分别代表资源名称类型（resource name type）
      -- 与事件类型（event type）。
      -- 返回值 `EventM` 是一个单子（monad），通过 `MonadState` typeclass 来访问与修改应用的状态。更推荐的方式则是
      -- 通过 `microlens-mtl` 库来更新准确的状态。
      --
      -- 默认情况下，当事件句柄完成后，Brick 调用 `appDraw` 重新绘制界面，并开始等待下一个事件。除此还有另外两种选项：
      -- 1. `Brick.Main.halt`：终止事件循环，其返回的状态传递给 `defaultMain` 或 `customMain`。
      -- 2. `Brick.Main.continueWithoutRedraw`：继续执行事件循环，直到另一个输入事件后再使用新状态进行绘制。
      --
      -- `EventM` 单子是 `IO` 的一个 transformer，因此可以通过 `liftIO` 来进行 I/O 操作。事件句柄应该立即执行来避免
      -- 重绘的延时，因此需要使用后台线程来异步的处理事件。
      --
      -- 顶层的 `appHandleEvent` 句柄负责管理应用的状态，同样也需要更新关联了 UI 组件的状态。
      appHandleEvent = resizeOrQuit,
      -- 当一个应用启动时，它可能会负责某些事件的到来，例如初始化滚动视图的状态。由于这些行为只能发生在 `EventM` 中，
      -- 并且我们不想等到第一个事件来临时再进行 `appHandleEvent`，`App` 类型提供了 `appStartEvent` 函数来负责此事。;
      --
      -- 该函数是一个运行在初始应用状态上的句柄行为。该函数仅执行一次，即应用启动时。该函数一般用于初始的滚动视图请求
      -- 或者修改 Vty 环境。多数情况下使用 `return ()` 即可。
      appStartEvent = return (),
      -- TODO
      appChooseCursor = const . const Nothing,
      appAttrMap = const $ attrMap defAttr []
    }

appDraw' :: AppState -> [Widget String]
appDraw' _ =
  [ str "Hello," <=> str "World!" <=> hBorder,
    hBox [str "                      Woohoo"]
  ]

{-
  将 `App` 传递给 `Brick.Main.defaultMain` 或是 `Brick.Main.customMain` 并带上初始应用状态，即可运行。
-}
main :: IO ()
main = do
  let initialState = 5
  defaultMain app initialState >>= print
