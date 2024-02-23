{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: BorderDemo.hs
-- author: Jacob Xie
-- date: 2024/02/23 08:58:16 Friday
-- brief:

module Main where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (Widget)
import Brick.Util (fg, on)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core
  ( hBox,
    hLimit,
    padLeftRight,
    str,
    txt,
    updateAttrMap,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Data.Text qualified as T
import Graphics.Vty qualified as V

styles :: [(T.Text, BS.BorderStyle)]
styles =
  [ ("ascii", BS.ascii),
    ("unicode", BS.unicode),
    ("unicode bold", BS.unicodeBold),
    ("unicode rounded", BS.unicodeRounded),
    ("custom", custom),
    ("from 'x'", BS.borderStyleFromChar 'x')
  ]

custom :: BS.BorderStyle
custom =
  BS.BorderStyle
    { BS.bsCornerTL = '/',
      BS.bsCornerTR = '\\',
      BS.bsCornerBR = '/',
      BS.bsCornerBL = '\\',
      BS.bsIntersectFull = '.',
      BS.bsIntersectL = '.',
      BS.bsIntersectR = '.',
      BS.bsIntersectT = '.',
      BS.bsIntersectB = '.',
      BS.bsHorizontal = '*',
      BS.bsVertical = '!'
    }

borderDemos :: [Widget ()]
borderDemos = mkBorderDemo <$> styles

mkBorderDemo :: (T.Text, BS.BorderStyle) -> Widget ()
mkBorderDemo (styleName, sty) =
  withBorderStyle sty $
    B.borderWithLabel (str "label") $
      vLimit 5 $
        C.vCenter $
          padLeftRight 2 $
            txt $
              styleName <> " style"

titleAttr :: A.AttrName
titleAttr = A.attrName "title"

attrs :: [(A.AttrName, V.Attr)]
attrs =
  [ (B.borderAttr, V.yellow `on` V.black),
    (B.vBorderAttr, fg V.cyan),
    (B.hBorderAttr, fg V.magenta),
    (titleAttr, fg V.cyan)
  ]

colorDemo :: Widget ()
colorDemo =
  updateAttrMap (A.applyAttrMappings attrs) $
    B.borderWithLabel (withAttr titleAttr $ str "title") $
      hLimit 20 $
        vLimit 5 $
          C.center $
            str "colors!"

ui :: Widget ()
ui =
  vBox
    [ hBox borderDemos,
      B.hBorder,
      colorDemo,
      B.hBorderWithLabel (str "horizontal border label"),
      C.center (str "Left of vertical border") <+> B.vBorder <+> C.center (str "Right of vertical border")
    ]

main :: IO ()
main = M.simpleMain ui
