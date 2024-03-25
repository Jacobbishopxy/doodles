{-# LANGUAGE TemplateHaskell #-}

-- file: Uppercase.hs
-- author: Jacob Xie
-- date: 2024/03/15 10:57:22 Friday
-- brief:
-- ref: https://github.com/andrevdm/bhoogle/blob/blog/app/Main.hs

module Main where

import Brick
import Brick qualified as T
import Brick.Focus qualified as F
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Monad (void)
import Data.Char (toUpper)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)

-- Controls
data Name = EditInput deriving (Show, Eq, Ord)

-- State datatype
data State = State
  { _focusRing :: F.FocusRing Name,
    _inputText :: Editor String Name,
    _outputText :: String
  }

makeLenses ''State

-- Event handler
handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case F.focusGetCurrent $ st ^. focusRing of
    Just EditInput -> do
      let foo = map toUpper $ concat $ getEditContents $ st ^. inputText
      outputText .= foo
    _ -> return ()
handleEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditInput -> zoom inputText $ handleEditorEvent ev
    Nothing -> return ()

-- Ui
drawUi :: State -> [Widget Name]
drawUi st = [ui]
  where
    e1 = F.withFocusRing (st ^. focusRing) (renderEditor (str . unlines)) (st ^. inputText)
    ui =
      center $
        (str "Input: " <+> hLimit 30 (vLimit 1 e1))
          <=> str " "
          <=> str (st ^. outputText)
          <=> str " "
          <=> str "Press Enter to convert string, Esc to quit."

-- Initial state
initialState :: State
initialState =
  State
    (F.focusRing [EditInput])
    (editor EditInput Nothing "")
    " "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.blue),
      (editFocusedAttr, V.black `on` V.yellow)
    ]

appCursor :: State -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

theApp :: App State e Name
theApp =
  App
    { appDraw = drawUi,
      appChooseCursor = appCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let vtyBuilder = mkVty V.defaultConfig

  initialVty <- vtyBuilder

  void $ customMain initialVty vtyBuilder Nothing theApp initialState

-- st <- defaultMain theApp initialState

-- putStrLn $ unlines $ getEditContents $ st ^. inputText
