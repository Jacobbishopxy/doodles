{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: RenderRingBuffer.hs
-- author: Jacob Xie
-- date: 2024/06/20 08:43:21 Thursday
-- brief:

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import OpsLib.ListRB

-- type synonym: outputDisplay
type DOut = ListRB () String

data AppState = AppState
  { _inputText :: Editor String (),
    _outputDisplay :: DOut
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUI :: AppState -> [Widget ()]
drawUI st = [ui <=> hint]
  where
    ed = renderEditor (str . unlines) False (st ^. inputText)
    box = vLimit 50 $ renderList (listDrawElement) True (getList $ st ^. outputDisplay)
    ui = center $ hLimit 100 $ vBox [vLimit 1 ed, str " ", box]
    hint =
      hCenter $
        vBox
          [ str "Press Enter to append a string",
            str "Press Esc to quit."
          ]

listDrawElement :: Bool -> String -> Widget ()
listDrawElement _ e = str e

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "whatever"
