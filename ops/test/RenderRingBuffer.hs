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
drawUI st = [m <=> (info <+> hint)]
  where
    ed =
      borderWithLabel (str "input") $
        F.withFocusRing (st ^. focusRing) (renderEditor $ str . unlines) (st ^. inputText)
    box =
      borderWithLabel (str "display") $
        renderList (listDrawElement st) True (st ^. outputDisplay . bList)
    m = center $ vBox [ed, box]
    rb = st ^. outputDisplay . ringBuffer
    info =
      borderWithLabel (str "info") $
        hCenter $
          vBox
            [ str $ "rb maxSize: " <> show (getMaxSize rb),
              str $ "rb cacheSize: " <> show (getCacheSize rb),
              str $ "rb currentSize: " <> show (getCurrentSize rb),
              str $ "rb wasElementDropped: " <> show (wasElementDropped rb)
            ]
    hint =
      borderWithLabel (str "hint") $
        hCenter $
          vBox
            [ str " ",
              str "Press Enter to append a string",
              str "Press Esc to quit.",
              str " "
            ]

-- render list
listDrawElement :: AppState -> Bool -> String -> Widget Name
listDrawElement st sel e =
  case F.focusGetCurrent $ st ^. focusRing of
    Just DisplayList | sel -> withAttr selectedListAttr $ str e
    _ -> withAttr unselectedListAttr $ str e

----------------------------------------------------------------------------------------------------
-- Handler
----------------------------------------------------------------------------------------------------

appEvent :: BrickEvent Name () -> EventM Name AppState ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = switchRegion
appEvent (VtyEvent (V.EvKey V.KBackTab [])) = switchRegion
appEvent ev@(VtyEvent e) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditInput -> case e of
      V.EvKey V.KEnter [] -> handleInput
      _ -> zoom inputText $ handleEditorEvent ev
    Just DisplayList -> zoom (outputDisplay . bList) $ handleListEvent e
    _ -> return ()
appEvent _ = return ()

-- handle input
handleInput :: EventM Name AppState ()
handleInput = modify $ \st ->
  let inp = concat $ getEditContents $ st ^. inputText
   in if inp == ""
        then st
        else
          st
            & outputDisplay
            %~ appendList inp
            & inputText
            .~ editor EditInput (Just 1) ""

-- switch between EditInput & DisplayList
switchRegion :: EventM Name AppState ()
switchRegion = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditInput -> focusRing %= F.focusSetCurrent DisplayList
    Just DisplayList -> focusRing %= F.focusSetCurrent EditInput
    _ -> return ()

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

appendList :: e -> ListRB Name e -> ListRB Name e
appendList e lrb =
  let newRb = appendRingBuffer e $ lrb ^. ringBuffer
      l = lrb ^. bList
   in if wasElementDropped newRb
        then ListRB newRb $ list DisplayList (getRingBuffer newRb) 1
        else ListRB newRb $ listInsert (length l) e l

createList :: Int -> ListRB Name e
createList s = ListRB (newRingBuffer s) (list DisplayList Vec.empty 1)

selectedListAttr :: AttrName
selectedListAttr = listSelectedAttr <> attrName "selectedList"

unselectedListAttr :: AttrName
unselectedListAttr = listAttr <> listSelectedAttr <> attrName "unselectedList"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.black `on` V.cyan),
      (editFocusedAttr, V.black `on` V.yellow),
      (selectedListAttr, V.black `on` V.yellow),
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
      _outputDisplay = createList 10
    }

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app initialState
