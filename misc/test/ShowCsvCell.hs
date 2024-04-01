{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: ShowCsvCell.hs
-- author: Jacob Xie
-- date: 2024/03/26 17:08:20 Tuesday
-- brief:

module Main where

import Brick
import Brick.Focus qualified as F
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Edit
  ( Editor,
    editAttr,
    editFocusedAttr,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
  )
import Brick.Widgets.List
import Brick.Widgets.Table
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (HasHeader), decode)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)

----------------------------------------------------------------------------------------------------
-- Csv
----------------------------------------------------------------------------------------------------

type Row = [String]

type CsvResult = Vec.Vector Row

readCsv :: FilePath -> IO CsvResult
readCsv file = do
  contents <- BL.readFile file
  case Data.Csv.decode HasHeader contents of
    Left err -> error $ "Parse CSV file error:\n" <> err
    Right ctt -> return ctt

countCsvCols :: CsvResult -> Int
countCsvCols = Prelude.length . Vec.head

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

data Name = EditInput | TableBox deriving (Show, Eq, Ord)

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    _inputText :: Editor String Name,
    _outputCsv :: List Name Row,
    _colIndex :: Int,
    _colNum :: Int
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- Tui
----------------------------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st = [ui <=> hint]
  where
    ed = F.withFocusRing (st ^. focusRing) (renderEditor (str . unlines)) (st ^. inputText)
    ui =
      center $
        hLimit 100 $
          vBox
            [ str "Csv file path: ",
              vLimit 1 ed,
              str " ",
              box
            ]
    hint =
      hCenter $
        vBox
          [ str "Press Enter to show csv.",
            str "Use arrow keys to change selection.",
            str "Press Esc to quit."
          ]

    label = str $ "Row " <> cur <> " / col " <> show (st ^. colIndex + 1)
    l = st ^. outputCsv
    box =
      borderWithLabel label $
        vLimit 50 $
          renderList (listDrawElement $ st ^. colIndex) True l
    cur = case l ^. listSelectedL of
      Nothing -> "-"
      Just i -> show (i + 1)

listDrawElement :: Int -> Bool -> Row -> Widget Name
listDrawElement colIdx sel row =
  let ws = (\c -> if c == "" then withDefAttr nullCellAttr (str "null") else str c) <$> row
      len = Prelude.length row
      maybeSelect es = selectCell <$> Prelude.zip [0 ..] es
      selectCell (i, w) = if sel && i == colIdx then withDefAttr selectedCellAttr w else w
   in hLimit 100 $
        hBox $
          maybeSelect $
            alignColumns (columnAlignments len) (columnWidths len) ws

selectedCellAttr :: AttrName
selectedCellAttr = attrName "selectedCell"

nullCellAttr :: AttrName
nullCellAttr = attrName "nullCell"

columnWidths :: Int -> [Int]
columnWidths = flip Prelude.replicate 10

columnAlignments :: Int -> [ColumnAlignment]
columnAlignments = flip Prelude.replicate AlignLeft

----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
-- press enter to read Csv
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case F.focusGetCurrent $ st ^. focusRing of
    Just EditInput -> do
      -- read csv
      res <- liftIO $ readCsv $ Prelude.concat $ getEditContents $ st ^. inputText
      outputCsv .= list TableBox res 1
      colNum .= countCsvCols res
      return ()
    _ -> return ()
-- switch between `Name`
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) =
  focusRing %= F.focusNext
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) =
  focusRing %= F.focusPrev
-- other actions
handleEvent ev@(VtyEvent e) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    -- editor events
    Just EditInput -> zoom inputText $ handleEditorEvent ev
    -- table events
    Just TableBox -> case e of
      V.EvKey V.KLeft [] ->
        colIndex %= (\i -> max 0 (i - 1))
      V.EvKey V.KRight [] -> do
        st <- get
        colIndex %= (\i -> min (st ^. colNum - 1) (i + 1))
      _ -> zoom outputCsv $ handleListEvent e
    _ -> return ()
handleEvent _ = return ()

----------------------------------------------------------------------------------------------------

app :: App AppState e Name
app =
  App
    { appDraw = drawUi,
      appChooseCursor = appCursor,
      appHandleEvent = handleEvent,
      appAttrMap = const theMap,
      appStartEvent = return ()
    }

initialState :: AppState
initialState =
  AppState
    { _focusRing = F.focusRing [EditInput, TableBox], -- init focusRing on needed UI
      _inputText = editor EditInput Nothing "",
      _outputCsv = list TableBox Vec.empty 0,
      _colIndex = 0,
      _colNum = 0
    }

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.black `on` V.cyan),
      (editFocusedAttr, V.black `on` V.yellow),
      (listAttr, V.white `on` V.blue),
      (selectedCellAttr, V.blue `on` V.white),
      -- null cell
      (nullCellAttr, style V.italic)
      -- (nullCellAttr, fg V.red)
    ]

appCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  -- Tui
  void $ customMain initialVty vtyBuilder Nothing app initialState
