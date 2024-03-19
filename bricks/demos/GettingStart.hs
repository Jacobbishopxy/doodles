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
      -- 注：这里的 `name` 用于细分时间类型，详见 `FormDemo.hs`
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
      -- `appChooseCursor` 光标放置
      -- 一个 `Widget` 的渲染过程可能会返回光标该如何放置的信息。例如一个文本编辑器需要报告光标的位置。然而 `Widget`
      -- 有可能会是一个很多光标放置组件的组合，我们需要一种方式选择哪个组件需要报告。
      --
      -- 为了确认是哪个光标被使用，或者哪个都不选择，我们需要设置 `App` 类型的 `appChooseCursor` 函数：
      -- `appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)`
      --
      -- 事件循环 event loop 通过渲染接口，收集由渲染过程产生的 `Brick.Types.CursorLocation` 值，并传递当前应用状态
      -- 给该函数。
      --
      -- 很多 widgets 在渲染过程中可以请求光标的放置，不过具体选择哪一个则是由应用决定的。由于只能选择展示至多一个光标在
      -- 终端上，我们需要决定具体展示的位置。一种办法是查看包含在 `cursorLocationName` 字段中的资源名称，关联了光标位置的
      -- 名称将会通过 `Brick.Widgets.Core.showCursor` 用于请求光标位置。
      --
      -- `Brick.Main` 提供了若干函数方便光标的选择：
      -- 1. `neverShowCursor`：永不展示
      -- 2. `showFirstCursor`：总是展示第一个给定的光标请求；方便仅有一个光标放置 widget 的应用
      -- 3. `showCursorNamed`：展示指定资源名称的光标，或是在没有任何名称被请求关联时不展示光标
      --
      -- 资源名称 resource names
      -- 上述资源名称用于描述光标位置，它们同样可以用于命名其它资源：
      -- 1. viewports
      -- 2. rendering extents
      -- 3. mouse events
      appChooseCursor = const . const Nothing,
      -- `appAttrMap` 管理属性
      -- `brick` 中使用 `appAttrMap` 将属性分配给接口的元素。在绘制 widget（例如 red-on-black text）时并非指定特定的属性，
      -- 而是指定一个属性名称，它是一个抽象的名称，例如 "keyword" 或 "e-mail address"。接着提供属性映射将这些属性名称映射至
      -- 确切的属性。该方法允许我们：
      -- 1. 运行时修改属性，允许用户任意更改应用程序中任何元素的属性，而无需强制构建特殊机制使其可配置
      -- 2. 编写例程用以从磁盘中加载已保存的属性映射
      -- 3. 为第三方组件提供模块化属性行为，我们不希望为了改变属性而重新编译第三方代码，也不希望将属性参数传递给第三方绘图函数
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
