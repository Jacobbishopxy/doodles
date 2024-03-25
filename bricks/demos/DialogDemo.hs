{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: DialogDemo.hs
-- author: Jacob Xie
-- date: 2024/03/25 13:14:28 Monday
-- brief:

module Main where

import Brick
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

data Choice = Red | Blue | Green deriving (Show)

data Name
  = RedButton
  | BlueButton
  | GreenButton
  deriving (Show, Eq, Ord)

drawUi :: D.Dialog Choice Name -> [Widget Name]
drawUi d = [ui]
  where
    ui = D.renderDialog d $ hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: BrickEvent Name e -> EventM Name (D.Dialog Choice Name) ()
appEvent (VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> halt
  V.EvKey V.KEnter [] -> halt
  _ -> D.handleDialogEvent ev
appEvent _ = return ()

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ str "Title") (Just (RedButton, choices)) 50
  where
    choices =
      [ ("Red", RedButton, Red),
        ("Blue", BlueButton, Blue),
        ("Green", GreenButton, Green)
      ]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.blue)
    ]

theApp :: App (D.Dialog Choice Name) e Name
theApp =
  App
    { appDraw = drawUi,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  d <- defaultMain theApp initialState
  putStrLn $ "You chose: " <> show (D.dialogSelection d)
