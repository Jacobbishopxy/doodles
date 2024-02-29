{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: TableDemo.hs
-- author: Jacob Xie
-- date: 2024/02/28 22:09:06 Wednesday
-- brief:

module Main where

import Brick
  ( Padding (Pad),
    Widget,
    padLeft,
    simpleMain,
    txt,
    (<+>),
    (<=>),
  )
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
  ( ColumnAlignment (AlignCenter),
    Table,
    alignCenter,
    alignMiddle,
    alignRight,
    columnBorders,
    renderTable,
    rowBorders,
    setDefaultColAlignment,
    surroundingBorder,
    table,
  )

ui :: Widget ()
ui =
  center $
    renderTable leftTable
      <+> padLeft (Pad 5) (renderTable rightTableA <=> renderTable rightTableB <=> renderTable rightTableC)

innerTable :: Table ()
innerTable =
  surroundingBorder False $
    table
      [ [txt "inner", txt "table"],
        [txt "is", txt "here"]
      ]

leftTable :: Table ()
leftTable =
  alignCenter 1 $
    alignRight 2 $
      alignMiddle 2 $
        table
          [ [txt "Left", txt "Center", txt "Right"],
            [txt "X", txt "Some things", txt "A"],
            [renderTable innerTable, txt "are", txt "B"],
            [txt "Z", txt "centered", txt "C"]
          ]

rightTableA :: Table ()
rightTableA =
  rowBorders False $
    setDefaultColAlignment AlignCenter $
      table
        [ [txt "A", txt "without"],
          [txt "table", txt "row borders"]
        ]

rightTableB :: Table ()
rightTableB =
  columnBorders False $
    setDefaultColAlignment AlignCenter $
      table
        [ [txt "A", txt "table"],
          [txt "without", txt "column borders"]
        ]

rightTableC :: Table ()
rightTableC =
  surroundingBorder False $
    rowBorders False $
      columnBorders False $
        setDefaultColAlignment AlignCenter $
          table
            [ [txt "A", txt "table"],
              [txt "without", txt "any borders"]
            ]

main :: IO ()
main = simpleMain ui
