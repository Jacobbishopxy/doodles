{-# LANGUAGE TemplateHaskell #-}

-- file: SysProc.hs
-- author: Jacob Xie
-- date: 2024/04/15 15:55:49 Monday
-- brief:

module Main where

import Brick
import Brick.Focus qualified as F
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.IO
import System.Process

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

data Name = EditInput deriving (Show, Eq, Ord)

data State = State
  { _focusRing :: F.FocusRing Name,
    _inputEditor :: Editor String Name,
    _outputText :: String
  }

makeLenses ''State

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUI :: State -> [Widget Name]
drawUI st = [ui]
  where
    e1 = F.withFocusRing (st ^. focusRing) (renderEditor $ str . unlines) (st ^. inputEditor)
    ui =
      center $
        vBox
          [ str "Input: " <+> hLimit 30 (vLimit 1 e1),
            str " ",
            str "Output: " <+> txt (T.pack $ st ^. outputText),
            str " ",
            str "Press Enter to finish input, Esc to quit."
          ]

----------------------------------------------------------------------------------------------------
-- Event
----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditInput -> do
      st' <- get
      let cmd = getEditContents $ st' ^. inputEditor
      p <- liftIO $ createProcess (proc "echo" cmd) {std_out = CreatePipe, std_in = CreatePipe}

      case p of
        (_, Just otp, _, pHandle) -> do
          -- block until the response is received
          contents <- liftIO $ hGetContents' otp

          outputText .= contents

          void $ liftIO $ waitForProcess pHandle
        _ -> return ()
    _ -> return ()
handleEvent ev = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditInput -> zoom inputEditor $ handleEditorEvent ev
    _ -> return ()

----------------------------------------------------------------------------------------------------
-- App
----------------------------------------------------------------------------------------------------

initialState :: State
initialState =
  State
    { _focusRing = F.focusRing [EditInput],
      _inputEditor = editor EditInput Nothing "",
      _outputText = ""
    }

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.blue),
      (editFocusedAttr, V.black `on` V.yellow)
    ]

appCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

theApp :: App State e Name
theApp =
  App
    { appDraw = drawUI,
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
