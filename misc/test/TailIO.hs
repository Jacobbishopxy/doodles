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
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
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
drawUI = undefined

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
  undefined
handleEvent _ = return ()

----------------------------------------------------------------------------------------------------
-- App
----------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.black),
      (editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

switchFormFocus :: Name -> Int -> Name
switchFormFocus =
  loopingList
    [ SleeperLB,
      SleeperUB,
      SleeperE,
      SleeperT
    ]

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "whatever"
