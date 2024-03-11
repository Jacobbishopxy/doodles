{-# LANGUAGE CPP #-}

-- file: CacheDemo.hs
-- author: Jacob Xie
-- date: 2024/03/11 09:04:17 Monday
-- brief:

module Main where

import Brick (AttrName, Padding (Pad), Widget, attrMap, attrName, cached, on, padBottom, padTopBottom, str, vBox, withDefAttr)
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Center qualified as C
import Control.Monad (void)
import Control.Monad.State (modify)
import Graphics.Vty qualified as V

data Name = ExpensiveWidget deriving (Ord, Show, Eq)

drawUi :: Int -> [Widget Name]
drawUi i = [ui]
  where
    ui =
      C.vCenter $
        vBox $
          C.hCenter
            <$> [ str "This demo shows how cached widgets behave. The top widget below",
                  str "is cacheable, so once it's rendered, brick re-uses the rendering",
                  str "each time it is drawn. The bottom widget is not cacheable so it is",
                  str "drawn on every request. Brick supports cache invalidation to force",
                  str "a redraw of cached widgets; we can trigger that here with 'i'. Notice",
                  str "how state changes with '+' aren't reflected in the cached widget",
                  str "until the cache is invalidated with 'i'.",
                  padTopBottom 1 $
                    cached ExpensiveWidget $
                      withDefAttr emphAttr $
                        str $
                          "This widget is cached (state = " <> show i <> ")",
                  padBottom (Pad 1) $
                    withDefAttr emphAttr $
                      str $
                        "This widget is not cached (state = " <> show i <> ")",
                  C.hCenter $ str "Press 'i' to invalidate the cache,",
                  str "'+' to change the state value, and",
                  str "'Esc' to quit."
                ]

emphAttr :: AttrName
emphAttr = attrName "emphasis"

appEvent :: T.BrickEvent Name e -> T.EventM Name Int ()
appEvent (T.VtyEvent (V.EvKey (V.KChar '+') [])) = modify (+ 1)
appEvent (T.VtyEvent (V.EvKey (V.KChar 'i') [])) = M.invalidateCacheEntry ExpensiveWidget
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

app :: M.App Int e Name
app =
  M.App
    { M.appDraw = drawUi,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const $ attrMap V.defAttr [(emphAttr, V.white `on` V.blue)],
      M.appChooseCursor = M.neverShowCursor
    }

main :: IO ()
main = void $ M.defaultMain app 0
