{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: CronSearch.hs
-- author: Jacob Xie
-- date: 2024/04/02 13:05:26 Tuesday
-- brief:

module Main where

import Brick
import Brick.Focus qualified as F
import Brick.Forms
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.List
  ( List,
    handleListEvent,
    list,
    listAttr,
    listSelectedAttr,
    renderList,
  )
import Brick.Widgets.Table
  ( ColumnAlignment (AlignLeft),
    alignColumns,
  )
import Control.Monad (void)
import CronSearchUtil
import Data.Either (fromRight)
import Data.List (elemIndex, (!?))
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Data.Yaml qualified as Y
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import MiscLib.CronSchema
import System.Environment (getArgs)

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

-- Source Name
data Name
  = SearchRegion SearchRegion
  | ResultHeaderRegion
  | ResultRegion
  | DetailRegion
  deriving (Eq, Ord, Show)

data SearchRegion
  = StringField
  | SelectSleeperField
  | SelectInputField
  | SelectCmdField
  | SelectOutputField
  | ConjAndField
  | ConjOrField
  | CaseSensitiveField
  | InvisibleField
  deriving (Eq, Ord, Show)

data ConjField = ConjAnd | ConjOr
  deriving (Show, Ord, Eq)

data Search = Search
  { _searchString :: T.Text,
    -- columns to search
    _selectSleeperCol :: Bool,
    _selectInputCol :: Bool,
    _selectCmdCol :: Bool,
    _selectOutputCol :: Bool,
    -- conjunction
    _conjunction :: Conj,
    -- ignore case
    _caseSensitive :: Bool,
    -- hidden widget, used when switched out from SearchRegion
    _invisibleFocus :: Bool
  }

makeLenses ''Search

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    -- search form
    _searchForm :: Form Search () Name,
    -- all crons
    _allCrons :: [CronSchema],
    -- searched result
    _searchedResult :: [CronSchema],
    _searchedResultList :: List Name CronSchema,
    -- searched result
    _selectedResult :: Int
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st = [ui]
  where
    ui =
      vBox
        [ vLimit 14 $
            hBox
              [ hLimitPercent 60 $ borderWithLabel titleSP $ controlBox st,
                borderWithLabel titleHP $ hCenter $ vCenter helpBox
              ],
          vLimitPercent 80 $ borderWithLabel titleMR $ resultBox st,
          borderWithLabel titleDI $ infoBox st <+> fill ' '
        ]
    titleSP = str "search param"
    titleHP = str "help"
    titleDI = str "detailed info"
    titleMR = str "matched result"

-- control box
controlBox :: AppState -> Widget Name
controlBox st = renderForm (st ^. searchForm)

-- help box
helpBox :: Widget Name
helpBox =
  str $
    "Arrow:   move up/down\n"
      <> "Space:   select param\n"
      <> "Enter:   search\n"
      <> "Tab:     switch panel\n"
      <> "Esc:     quit"

-- result box
resultBox :: AppState -> Widget Name
resultBox st =
  vLimit 1 (renderList listDrawResultHeader False h) <=> r
  where
    -- header
    h = list ResultHeaderRegion (Vec.fromList [resultBoxColumns]) 1
    l = st ^. searchedResultList
    -- if not on focus, disable highlight
    r = case F.focusGetCurrent $ st ^. focusRing of
      -- when focus ring on
      Just ResultRegion -> renderList listDrawResult True l
      -- when focus ring off
      _ -> withAttr resultUnselectedListAttr $ renderList listDrawResult' True l

-- info box
infoBox :: AppState -> Widget Name
infoBox st =
  case (st ^. searchedResult) !? (st ^. selectedResult) of
    Nothing -> emptyWidget
    Just cs -> renderList listDrawInfo False $ list DetailRegion (l cs) 2
  where
    -- generate info list
    g :: CronSchema -> [String] -> [String]
    g cs c = [c' <> ": " <> s' | (c', s') <- zip c (getCronStrings cs c)]
    l :: CronSchema -> Vec.Vector String
    l = Vec.fromList . flip g resultBoxColumns

----------------------------------------------------------------------------------------------------

-- form builder
mkForm :: Search -> Form Search e Name
mkForm =
  newForm
    [ labelP "Lookup string" @@= editTextField searchString (SearchRegion StringField) (Just 1),
      label "Select columns" @@= checkboxField selectSleeperCol (SearchRegion SelectSleeperField) "Sleeper",
      label "" @@= checkboxField selectInputCol (SearchRegion SelectInputField) "Input",
      label "" @@= checkboxField selectCmdCol (SearchRegion SelectCmdField) "Cmd",
      labelP "" @@= checkboxField selectOutputCol (SearchRegion SelectOutputField) "Output",
      labelP "Conjunction" @@= radioField conjunction radioG,
      labelP "Case sensitive" @@= checkboxField caseSensitive (SearchRegion CaseSensitiveField) "",
      labelI @@= checkboxField invisibleFocus (SearchRegion InvisibleField) ""
    ]
  where
    label s w = vLimit 1 (hLimit 20 $ str s <+> fill ' ') <+> w
    labelP s w = padBottom (Pad 1) $ label s w
    labelI = withAttr invisibleFormFieldAttr
    radioG = [(AND, SearchRegion ConjAndField, "And"), (OR, SearchRegion ConjOrField, "Or")]

-- result header
listDrawResultHeader :: Bool -> [String] -> Widget Name
listDrawResultHeader _ cs =
  withAttr resultHeaderListAttr $
    hBox $
      alignColumns columnAlignments columnWidths $
        str <$> cs

-- draw an item in `[CronSchema]` list
listDrawResult :: Bool -> CronSchema -> Widget Name
listDrawResult sel cs =
  let ws = if sel then s else str <$> c
   in hBox $ alignColumns columnAlignments columnWidths ws
  where
    c = getCronStrings cs resultBoxColumns
    s = withAttr resultSelectedListAttr . str <$> c

listDrawResult' :: Bool -> CronSchema -> Widget Name
listDrawResult' _ cs =
  hBox $ alignColumns columnAlignments columnWidths $ str <$> c
  where
    c = getCronStrings cs resultBoxColumns

-- fixed length
-- ["idx", "dag", "name", "sleeper", "input", "cmd", "output", "activate", "fPath"]
columnWidths :: [Int]
columnWidths = [5, 15, 15, 10, 40, 40, 40, 5, 40]

columnAlignments :: [ColumnAlignment]
columnAlignments = replicate (length resultBoxColumns) AlignLeft

listDrawInfo :: Bool -> String -> Widget Name
listDrawInfo True = withAttr detailSelectedListAttr . str
listDrawInfo _ = str

----------------------------------------------------------------------------------------------------
-- Event
----------------------------------------------------------------------------------------------------

appEvent :: BrickEvent Name () -> EventM Name AppState ()
-- quit
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
-- press Tab/BackTab switch to next panel
appEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = switchRegion
appEvent (VtyEvent (V.EvKey V.KBackTab [])) = switchRegion
-- press Enter to search while in `SearchRegion`
appEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just (SearchRegion _) -> modify commitSearchRequest
    _ -> return ()
-- press arrow Up
appEvent (VtyEvent k@(V.EvKey V.KUp [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    -- arrow up/down effects detailed info
    Just ResultRegion -> do
      zoom searchedResultList $ handleListEvent k
      modify $ \st ->
        if st ^. selectedResult > 0 then st & selectedResult -~ 1 else st
    -- move to the previous form focus
    Just (SearchRegion f) -> do
      let f' = SearchRegion $ formFocusRingLoop f
      focusRing %= F.focusSetCurrent f'
      modify $ searchForm %~ setFormFocus f'
    _ -> return ()
-- press arrow Down
appEvent (VtyEvent k@(V.EvKey V.KDown [])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    -- arrow up/down effects detailed info
    Just ResultRegion -> do
      zoom searchedResultList $ handleListEvent k
      modify $ \st ->
        if st ^. selectedResult < length (st ^. searchedResult) - 1 then st & selectedResult +~ 1 else st
    -- move to the next form focus
    Just (SearchRegion f) -> do
      let f' = SearchRegion $ formFocusRingLoop' f
      focusRing %= F.focusSetCurrent f'
      modify $ searchForm %~ setFormFocus f'
    _ -> return ()
-- other cases
appEvent ev@(VtyEvent ve) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just (SearchRegion _) -> zoom searchForm $ handleFormEvent ev
    Just ResultRegion -> zoom searchedResultList $ handleListEvent ve
    _ -> return ()
appEvent _ = return ()

-- switch between SearchRegion and ResultRegion
switchRegion :: EventM Name AppState ()
switchRegion = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just (SearchRegion _) -> do
      focusRing %= F.focusSetCurrent ResultRegion
      modify $ searchForm %~ setFormFocus (SearchRegion InvisibleField)
    Just ResultRegion -> do
      focusRing %= F.focusSetCurrent (SearchRegion StringField)
      modify $ searchForm %~ setFormFocus (SearchRegion StringField)
    _ -> return ()

-- according to the current form states, update filtered result
commitSearchRequest :: AppState -> AppState
commitSearchRequest st =
  let sp = genSearchParam $ formState $ st ^. searchForm
      sr = searchCron sp (st ^. allCrons)
   in st
        & searchedResult .~ sr
        & searchedResultList .~ list ResultRegion (Vec.fromList sr) 2
        & searchForm %~ setFormFocus (SearchRegion InvisibleField) -- set form focus to null
        & focusRing %~ F.focusSetCurrent ResultRegion -- jump to result region

genSearchParam :: Search -> SearchParam
genSearchParam s =
  let flt = [s ^. selectSleeperCol, s ^. selectInputCol, s ^. selectCmdCol, s ^. selectOutputCol]
      cols = ["sleeper", "input", "cmd", "output"]
      sf = [c | (c, f) <- zip cols flt, f]
   in SearchParam sf (s ^. conjunction) (T.unpack $ s ^. searchString)

----------------------------------------------------------------------------------------------------
-- Attr
----------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.black),
      (editFocusedAttr, V.black `on` V.yellow),
      (listAttr, V.white `Brick.on` V.black),
      (listSelectedAttr, V.black `Brick.on` V.yellow),
      (formAttr, V.white `Brick.on` V.black),
      (focusedFormInputAttr, V.black `on` V.yellow),
      -- overwrite
      (invisibleFormFieldAttr, fg V.black),
      (resultHeaderListAttr, V.white `on` V.blue),
      (resultSelectedListAttr, V.black `on` V.yellow),
      (resultUnselectedListAttr, V.white `on` V.black),
      (detailSelectedListAttr, V.white `on` V.black)
    ]

invisibleFormFieldAttr :: AttrName
invisibleFormFieldAttr = focusedFormInputAttr <> attrName "invisibleFormField"

resultHeaderListAttr :: AttrName
resultHeaderListAttr = listAttr <> listSelectedAttr <> attrName "resultHeaderList"

resultSelectedListAttr :: AttrName
resultSelectedListAttr = listSelectedAttr <> attrName "resultSelectedList"

resultUnselectedListAttr :: AttrName
resultUnselectedListAttr = listAttr <> listSelectedAttr <> attrName "resultUnselectedList"

detailSelectedListAttr :: AttrName
detailSelectedListAttr = listSelectedAttr <> attrName "detailSelectedList"

----------------------------------------------------------------------------------------------------
-- App
----------------------------------------------------------------------------------------------------

app :: App AppState () Name
app =
  App
    { appDraw = drawUi,
      appChooseCursor = appCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

defaultSearch :: Search
defaultSearch =
  Search
    { _searchString = "",
      _selectSleeperCol = False,
      _selectInputCol = True,
      _selectCmdCol = True,
      _selectOutputCol = True,
      _conjunction = OR,
      _caseSensitive = False,
      _invisibleFocus = False
    }

focusRingList :: [Name]
focusRingList =
  [ SearchRegion StringField,
    SearchRegion SelectSleeperField,
    SearchRegion SelectInputField,
    SearchRegion SelectCmdField,
    SearchRegion SelectOutputField,
    SearchRegion ConjAndField,
    SearchRegion ConjOrField,
    SearchRegion CaseSensitiveField,
    ResultRegion
  ]

formFocusRingList :: [SearchRegion]
formFocusRingList =
  [ StringField,
    SelectSleeperField,
    SelectInputField,
    SelectCmdField,
    SelectOutputField,
    ConjAndField,
    ConjOrField,
    CaseSensitiveField
  ]

-- key up
formFocusRingLoop :: SearchRegion -> SearchRegion
formFocusRingLoop f = case f `elemIndex` formFocusRingList of
  Just 0 -> CaseSensitiveField
  Just i -> formFocusRingList !! (i - 1)
  _ -> error "formFocusRingLoop"

-- key down
formFocusRingLoop' :: SearchRegion -> SearchRegion
formFocusRingLoop' f = case f `elemIndex` formFocusRingList of
  Just 7 -> StringField
  Just i -> formFocusRingList !! (i + 1)
  _ -> error "formFocusRingLoop'"

initialState :: [CronSchema] -> AppState
initialState cs =
  AppState
    { _focusRing = F.focusRing focusRingList,
      _searchForm = mkForm defaultSearch,
      _allCrons = cs,
      _searchedResult = [],
      _searchedResultList = list ResultRegion Vec.empty 0,
      _selectedResult = 0
    }

appCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  -- read yaml file and load all crons
  let yamlPath = case args of
        (x : _) -> x
        [] -> "./cron_settings.yml"
  p <- Y.decodeFileEither yamlPath
  let s = fromRight (error "check yaml if exists") p
  crons <- getAllCrons $ lookupDirs s

  -- build vty
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  -- Tui
  void $ customMain initialVty vtyBuilder Nothing app (initialState crons)
