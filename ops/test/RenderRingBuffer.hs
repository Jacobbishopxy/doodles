{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: RenderRingBuffer.hs
-- author: Jacob Xie
-- date: 2024/06/20 08:43:21 Thursday
-- brief:

module Main where

import Brick
import qualified Brick.Focus as F
import Brick.Widgets.Border
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import OpsLib.RingBuffer

----------------------------------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------------------------------

data Name = EditInput | DisplayList deriving (Show, Eq, Ord)

data ListRB n e = ListRB
  { _ringBuffer :: RingBuffer e,
    _bList :: List n e
  }

makeLenses ''ListRB

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    _inputText :: Editor String Name,
    _outputDisplay :: ListRB Name String
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUI :: AppState -> [Widget Name]
drawUI st = [ui <=> hint]
  where
    ed = F.withFocusRing (st ^. focusRing) (renderEditor $ str . unlines) (st ^. inputText)
    box = borderWithLabel (str "display") $ renderList (listDrawElement) True (st ^. outputDisplay . bList)
    ui = center $ vBox [vLimit 1 ed, str " ", box]
    hint =
      borderWithLabel (str "hint") $
        hCenter $
          vBox
            [ str "Press Enter to append a string",
              str "Press Esc to quit."
            ]

listDrawElement :: Bool -> String -> Widget Name
listDrawElement sel e =
  if sel
    then withAttr selectedListAttr $ str e
    else withAttr unselectedListAttr $ str e

----------------------------------------------------------------------------------------------------
-- Handler
----------------------------------------------------------------------------------------------------

appEvent :: BrickEvent Name () -> EventM Name AppState ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent ev@(VtyEvent e) = do
  r <- use focusRing
  s <- get
  case F.focusGetCurrent r of
    Just EditInput -> case e of
      V.EvKey V.KEnter [] -> do
        liftIO $ putStrLn $ "rb: " <> show (s ^. outputDisplay . ringBuffer)
        modify $ \st ->
          st
            & outputDisplay
            %~ appendList (concat $ getEditContents $ st ^. inputText)
            & inputText
            .~ editor EditInput (Just 1) ""
      _ -> zoom inputText $ handleEditorEvent ev
    Just DisplayList -> zoom (outputDisplay . bList) $ handleListEvent e
    _ -> return ()
appEvent _ = return ()

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

appendList :: e -> ListRB Name e -> ListRB Name e
appendList e lrb =
  let newRb = appendRingBuffer e $ lrb ^. ringBuffer
      l = lrb ^. bList
   in if wasElementDropped newRb
        then ListRB newRb $ list DisplayList (getRingBuffer newRb) 1
        else ListRB newRb $ listReplace (getRingBuffer newRb) (Just 0) l

createList :: Int -> Int -> ListRB Name e
createList h s = ListRB (newRingBuffer s) (list DisplayList Vec.empty h)

selectedListAttr :: AttrName
selectedListAttr = listSelectedAttr <> attrName "selectedList"

unselectedListAttr :: AttrName
unselectedListAttr = listAttr <> listSelectedAttr <> attrName "unselectedList"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (selectedListAttr, V.black `on` V.yellow),
      (unselectedListAttr, V.white `on` V.black)
    ]

app :: App AppState () Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

initialState :: AppState
initialState =
  AppState
    { _focusRing = F.focusRing [EditInput, DisplayList],
      _inputText = editor EditInput (Just 1) "",
      _outputDisplay = createList 1 2
    }

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initialState
