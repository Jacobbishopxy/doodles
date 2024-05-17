{-# LANGUAGE TemplateHaskell #-}

-- file: TailIO.hs
-- author: Jacob Xie
-- date: 2024/04/18 14:36:25 Thursday
-- brief:

module Main where

import Brick
import Brick.Focus qualified as F
import Brick.Forms
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import MiscLib (loopingList)
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

data Name
  = SleeperLB
  | SleeperUB
  | SleeperE
  | SleeperT
  deriving (Show, Eq, Ord)

data OpForm = OpForm
  { _sleeperLowerBound :: Int,
    _sleeperUpperBound :: Int,
    _sleeperExists :: Int,
    _sleeperTimeout :: Int
  }

makeLenses ''OpForm

data State = State
  { _focusRing :: F.FocusRing Name,
    _opForm :: Form OpForm () Name,
    _outputText :: String
  }

makeLenses ''State

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUI :: State -> [Widget Name]
drawUI st = [ui]
  where
    ui = center $ vBox [controlBox st, txt $ T.pack $ st ^. outputText]

-- control box
controlBox :: State -> Widget Name
controlBox st = renderForm (st ^. opForm)

----------------------------------------------------------------------------------------------------

mkForm :: OpForm -> Form OpForm () Name
mkForm =
  newForm
    [ label "Sleeper lower bound" @@= editShowableField sleeperLowerBound SleeperLB,
      label "Sleeper upper bound" @@= editShowableField sleeperUpperBound SleeperUB,
      label "Sleeper exists" @@= editShowableField sleeperExists SleeperE,
      labelP "Sleeper timeout" @@= editShowableField sleeperTimeout SleeperT
    ]
  where
    label s w = vLimit 1 (hLimit 20 $ str s <+> fill ' ') <+> w
    labelP s w = padBottom (Pad 1) $ label s w

----------------------------------------------------------------------------------------------------
-- Event
----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name () -> EventM Name State ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just f -> do
      let f' = switchFormFocus f (-1)
      focusRing %= F.focusSetCurrent f'
      modify $ opForm %~ setFormFocus f'
    _ -> return ()
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just f -> do
      let f' = switchFormFocus f 1
      focusRing %= F.focusSetCurrent f'
      modify $ opForm %~ setFormFocus f'
    _ -> return ()
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st' <- get
  let fm = formState $ st' ^. opForm
      lb = fm ^. sleeperLowerBound
      ub = fm ^. sleeperUpperBound
      ex = fm ^. sleeperExists
      tot = fm ^. sleeperTimeout
  (_, output, err, hd) <-
    liftIO $
      createProcess
        (proc "bash" ["scripts/rand_print.sh", "-l", show lb, "-h", show ub, "-m", show ex])
          { std_out = CreatePipe,
            std_err = CreatePipe
          }

  -- TODO: concurrent
  -- mVar <- liftIO newEmptyMVar
  -- let output' = fromMaybe (error "output is None") output

  -- tid <- liftIO $ forkIO $ hGetLine output' >>= putMVar mVar

  -- let loop = do
  --       result <- timeout tot $ takeMVar mVar
  --       maybe loop return result

  -- x <- liftIO loop

  -- liftIO $ killThread tid

  -- get output contents
  contents <- liftIO $ mapM hGetContents output

  -- get error messages
  errors <- liftIO $ mapM hGetContents err
  -- get exit code
  exitCode <- liftIO $ waitForProcess hd

  -- modify state
  case exitCode of
    ExitFailure _ -> outputText .= fromMaybe "" errors
    ExitSuccess -> outputText .= fromMaybe "" contents
handleEvent ev =
  zoom opForm $ handleFormEvent ev

----------------------------------------------------------------------------------------------------

commitFormRequest :: OpForm -> State
commitFormRequest = undefined

----------------------------------------------------------------------------------------------------
-- App
----------------------------------------------------------------------------------------------------

initialState :: State
initialState =
  State
    { _focusRing = F.focusRing focusList,
      _opForm =
        mkForm
          OpForm
            { _sleeperLowerBound = 1,
              _sleeperUpperBound = 5,
              _sleeperExists = 10,
              _sleeperTimeout = 10
            },
      _outputText = ""
    }

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.black),
      (editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
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

focusList :: [Name]
focusList =
  [ SleeperLB,
    SleeperUB,
    SleeperE,
    SleeperT
  ]

switchFormFocus :: Name -> Int -> Name
switchFormFocus = loopingList focusList

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  void $ customMain initialVty vtyBuilder Nothing theApp initialState
