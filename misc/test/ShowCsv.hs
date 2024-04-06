{-# LANGUAGE OverloadedStrings #-}

-- file: ShowCsv.hs
-- author: Jacob Xie
-- date: 2024/03/12 12:20:06 Tuesday
-- brief:

module Main where

import Brick
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.List qualified as L
import Control.Monad (void)
import Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (HasHeader), decode)
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import System.Environment (getArgs)

----------------------------------------------------------------------------------------------------
-- Csv
----------------------------------------------------------------------------------------------------

type CsvResult = Vec.Vector T.Text

readCsv :: FilePath -> IO (Either String CsvResult)
readCsv file = do
  contents <- BL.readFile file
  return $ Vec.map cvtRow <$> decode HasHeader contents

-- convert vector of ByteString into Text
cvtRow :: Vec.Vector BL.ByteString -> T.Text
cvtRow = T.intercalate "," . Prelude.map (TE.decodeUtf8 . BL.toStrict) . Vec.toList

----------------------------------------------------------------------------------------------------
-- TUI
----------------------------------------------------------------------------------------------------

drawUi :: (Show a) => L.List () a -> [Widget ()]
drawUi l = [ui]
  where
    label = str "Item" <+> cur <+> str " of " <+> total
    cur = case l ^. L.listSelectedL of
      Nothing -> str "-"
      Just i -> str $ show (i + 1)
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box =
      B.borderWithLabel label $
        hLimit 100 $
          vLimit 50 $
            L.renderList listDrawElement True l
    ui =
      C.vCenter $
        vBox
          [ C.hCenter box,
            str " ",
            C.hCenter $ str "Press Esc to exit."
          ]

appEvent :: BrickEvent () e -> EventM () (L.List () T.Text) ()
appEvent (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt
    ev -> L.handleListEvent ev
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr $ str $ "<" <> s <> ">"
          else withAttr customAttr2 $ str s
   in C.hCenter $ str "Item" <+> selStr (show a)

customAttr :: AttrName
customAttr = L.listSelectedAttr <> attrName "custom"

customAttr2 :: AttrName
customAttr2 = L.listSelectedAttr <> attrName "custom2"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ --  (L.listAttr, V.white `on` V.blue),
      -- (L.listSelectedAttr, V.blue `on` V.white),
      -- (customAttr, fg V.cyan),
      (customAttr, V.blue `on` V.white),
      (customAttr2, V.white `on` V.blue)
    ]

theApp :: App (L.List () T.Text) e ()
theApp =
  App
    { appDraw = drawUi,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

initialState :: CsvResult -> L.List () T.Text
initialState a = L.list () a 1

-- test
printCsv :: Either String CsvResult -> IO ()
printCsv csv = case csv of
  Left err -> putStrLn $ "Error: " ++ err
  Right rawStrings -> do
    putStrLn "Raw strings from CSV file:"
    print rawStrings

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let csvFile = Prelude.head args
  result <- readCsv csvFile

  let ini = initialState $ fromRight Vec.empty result

  void $ defaultMain theApp ini

  printCsv result
