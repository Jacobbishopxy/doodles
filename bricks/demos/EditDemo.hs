{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: EditDemo.hs
-- author: Jacob Xie
-- date: 2024/02/27 08:51:05 Tuesday
-- brief:

module Main where

import Brick (hLimit, str, vLimit, (<+>), (<=>))
import Brick.AttrMap qualified as A
import Brick.Focus qualified as F
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Util (on)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom, (%=))
import Lens.Micro.TH (makeLenses)

data Name = Edit1 | Edit2 deriving (Ord, Show, Eq)

data St = St
  { _focusRing :: F.FocusRing Name,
    _edit1 :: E.Editor String Name,
    _edit2 :: E.Editor String Name
  }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. focusRing) (E.renderEditor (str . unlines)) (st ^. edit1)
    e2 = F.withFocusRing (st ^. focusRing) (E.renderEditor (str . unlines)) (st ^. edit2)
    ui =
      C.center $
        (str "Input 1 (unlimited): " <+> hLimit 30 (vLimit 5 e1))
          <=> str " "
          <=> (str "Input 2 (limited to 2 lines): " <+> hLimit 30 e2)
          <=> str " "
          <=> str "Press Tab to switch between editors, Esc to quit."

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
  M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
  focusRing %= F.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
  focusRing %= F.focusPrev
appEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just Edit1 -> zoom edit1 $ E.handleEditorEvent ev
    Just Edit2 -> zoom edit2 $ E.handleEditorEvent ev
    Nothing -> return ()

initialState :: St
initialState =
  St
    (F.focusRing [Edit1, Edit2])
    (E.editor Edit1 Nothing "")
    (E.editor Edit2 (Just 2) "")

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

theApp :: M.App St e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = appCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  st <- M.defaultMain theApp initialState
  putStrLn "In input 1 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. edit1
  putStrLn "In input 2 you entered:\n"
  putStrLn $ unlines $ E.getEditContents $ st ^. edit2
