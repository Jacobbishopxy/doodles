{-# LANGUAGE TemplateHaskell #-}

-- file: ListDir.hs
-- author: Jacob Xie
-- date: 2024/04/16 16:31:48 Tuesday
-- brief:

module Main where

import Brick
import Brick.Focus qualified as F
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.Exit
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
          [ str "Input: " <+> hLimit 50 (vLimit 1 e1),
            str " ",
            str "Output:" <=> txt (T.pack $ st ^. outputText),
            str " ",
            str "Press Enter to finish input, Esc to quit."
          ]

----------------------------------------------------------------------------------------------------
-- Event
----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name () -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just EditInput -> do
      st' <- get
      let lookupDir = getEditContents $ st' ^. inputEditor
      result <-
        liftIO $
          try' $ -- try is unnecessary here, since `ls` is a normal program -- try is unnecessary here, since `ls` is a normal program
            createProcess
              (proc "ls" $ ["-a", "-l"] <> lookupDir)
                { std_out = CreatePipe,
                  std_err = NoStream -- ignore error, make error display on Output field
                }
      case result of
        Left ex -> outputText .= show ex
        Right (_, o, _, h) -> do
          -- get output contents
          contents <- liftIO $ mapM hGetContents o
          exitCode <- liftIO $ waitForProcess h
          -- modify state
          case exitCode of
            ExitFailure _ -> outputText .= show (concat lookupDir) <> " is not accessible!"
            ExitSuccess -> outputText .= fromMaybe "" contents
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

theApp :: App State () Name
theApp =
  App
    { appDraw = drawUI,
      appChooseCursor = appCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

try' :: IO a -> IO (Either IOException a)
try' = try

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  void $ customMain initialVty vtyBuilder Nothing theApp initialState
