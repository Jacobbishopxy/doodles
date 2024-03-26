{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: ShowCsvPro.hs
-- author: Jacob Xie
-- date: 2024/03/15 09:48:10 Friday
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
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (HasHeader), decode)
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)

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
-- Tui
----------------------------------------------------------------------------------------------------

-- source name, representing different UI chunk
data Name = EditInput | CsvBox deriving (Show, Eq, Ord)

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    _inputText :: Editor String Name,
    _outputCsv :: List Name T.Text
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st = [ui]
  where
    ed = F.withFocusRing (st ^. focusRing) (renderEditor (str . unlines)) (st ^. inputText)
    ui =
      center $
        str "Csv file path: "
          <=> hLimit 100 (vLimit 2 ed)
          <=> str " "
          <=> box
          <=> str " "
          <=> str "Press Enter to show csv, Esc to quit."
    label = str "Index " <+> cur <+> str " of " <+> total
    l = st ^. outputCsv
    box =
      borderWithLabel label $
        hLimit 100 $
          vLimit 50 $
            renderList listDrawCsv True l
    cur = case l ^. listSelectedL of
      Nothing -> str "-"
      Just i -> str $ show (i + 1)
    total = str $ show $ Vec.length $ l ^. listElementsL

listDrawCsv :: Bool -> T.Text -> Widget Name
listDrawCsv sel a =
  let selStr s =
        if sel
          then withAttr listCsvAttr (str $ "-> " <> s)
          else str s
   in hCenter $ selStr (show a)

listCsvAttr :: AttrName
listCsvAttr = listSelectedAttr <> attrName "custom"

----------------------------------------------------------------------------------------------------

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
-- press enter to finish input
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case F.focusGetCurrent $ st ^. focusRing of
    Just EditInput -> do
      res <- liftIO $ readCsv $ Prelude.concat $ getEditContents $ st ^. inputText
      let s = fromRight Vec.empty res
      outputCsv .= list CsvBox s 1
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
    Just EditInput -> zoom inputText $ handleEditorEvent ev
    Just CsvBox -> zoom outputCsv $ handleListEventVi handleListEvent e
    _ -> return ()
handleEvent _ = return ()

--
appCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

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
    { _focusRing = F.focusRing [EditInput, CsvBox], -- init focusRing on needed UI
      _inputText = editor EditInput Nothing "",
      _outputCsv = list CsvBox Vec.empty 1
    }

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.blue),
      (editFocusedAttr, V.black `on` V.yellow),
      (listAttr, V.white `on` V.blue),
      (listSelectedAttr, V.blue `on` V.white),
      (listCsvAttr, fg V.cyan)
    ]

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  -- Tui
  void $ customMain initialVty vtyBuilder Nothing app initialState

-- /home/jacob/Code/doodles/tmp/test.csv
