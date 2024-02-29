{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: FileBrowserDemo.hs
-- author: Jacob Xie
-- date: 2024/02/29 08:40:04 Thursday
-- brief:

module Main where

import Brick
  ( Padding (Pad),
    Widget,
    emptyWidget,
    fg,
    hLimit,
    on,
    padTop,
    txt,
    vBox,
    vLimit,
    withDefAttr,
    (<=>),
  )
import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.FileBrowser qualified as FB
import Brick.Widgets.List qualified as L
import Control.Exception qualified as E
import Control.Monad.State (get)
import Data.Text qualified as Text
import Graphics.Vty qualified as V

data Name = FileBrowser1 deriving (Eq, Show, Ord)

drawUI :: FB.FileBrowser Name -> [Widget Name]
drawUI b = [center $ ui <=> help]
  where
    ui =
      hCenter $
        vLimit 15 $
          hLimit 50 $
            borderWithLabel (txt "Choose a file") $
              FB.renderFileBrowser True b
    help =
      padTop (Pad 1) $
        vBox
          [ case FB.fileBrowserException b of
              Nothing -> emptyWidget
              Just e ->
                hCenter $
                  withDefAttr errorAttr $
                    txt $
                      Text.pack $
                        E.displayException e,
            hCenter $ txt "Up/Down: select",
            hCenter $ txt "/: search, Ctrl-C or Esc: cancel search",
            hCenter $ txt "Enter: change directory or select file",
            hCenter $ txt "Esc: quit"
          ]

appEvent :: T.BrickEvent Name e -> T.EventM Name (FB.FileBrowser Name) ()
appEvent (T.VtyEvent ev) = do
  b <- get
  case ev of
    V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) -> M.halt
    _ -> do
      FB.handleFileBrowserEvent ev
      case ev of
        V.EvKey V.KEnter [] -> do
          b' <- get
          case FB.fileBrowserSelection b' of
            [] -> return ()
            _ -> M.halt
        _ -> return ()
appEvent _ = return ()

errorAttr :: A.AttrName
errorAttr = A.attrName "error"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.yellow),
      (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue),
      (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue),
      (FB.fileBrowserDirectoryAttr, fg V.blue),
      (FB.fileBrowserBlockDeviceAttr, fg V.magenta),
      (FB.fileBrowserCharacterDeviceAttr, fg V.green),
      (FB.fileBrowserNamedPipeAttr, fg V.yellow),
      (FB.fileBrowserSymbolicLinkAttr, fg V.cyan),
      (FB.fileBrowserUnixSocketAttr, fg V.red),
      (FB.fileBrowserSelectedAttr, V.white `on` V.magenta),
      (errorAttr, fg V.red)
    ]

theApp :: M.App (FB.FileBrowser Name) e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  b <- M.defaultMain theApp =<< FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing
  putStrLn $ "Selected entry: " <> show (FB.fileBrowserSelection b)
