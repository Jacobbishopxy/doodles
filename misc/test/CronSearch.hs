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
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.List
import Brick.Widgets.Table
import Control.Monad (void)
import Data.Aeson (FromJSON)
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Data.Yaml qualified as Y
import GHC.Generics (Generic)
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
  | ResultRegion
  | DetailRegion
  deriving (Eq, Ord, Show)

data SearchRegion
  = StringField
  | SelectInputField
  | SelectCmdField
  | SelectOutputField
  | ConjAndField
  | ConjOrField
  | CaseSensitiveField
  deriving (Eq, Ord, Show)

data ColsField = InputCol | CmdCol | OutputCol
  deriving (Show, Ord, Eq)

data ConjField = ConjAnd | ConjOr
  deriving (Show, Ord, Eq)

data Search = Search
  { _searchString :: T.Text,
    -- columns to search
    _selectInputCol :: Bool,
    _selectCmdCol :: Bool,
    _selectOutputCol :: Bool,
    -- conjunction
    _conjunction :: Conj,
    -- ignore case
    _caseSensitive :: Bool
  }

makeLenses ''Search

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    -- search form
    _search :: Search,
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
-- Const
----------------------------------------------------------------------------------------------------

resultBoxColumns :: [String]
resultBoxColumns = ["idx", "dag", "name", "sleeper", "input", "cmd", "output", "activate", "file"]

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st = [ui]
  where
    ui =
      vBox
        [ vLimit 13 $
            hBox
              [ hLimitPercent 70 $ borderWithLabel titleSP $ controlBox st,
                hCenter $ vCenter helpBox
              ],
          vLimitPercent 70 $ borderWithLabel titleMR $ resultBox st,
          borderWithLabel titleDI $ infoBox st <+> fill ' '
        ]
    titleSP = str "search param"
    titleDI = str "detailed info"
    titleMR = str "matched result"

-- control box
controlBox :: AppState -> Widget Name
controlBox st = renderForm (st ^. searchForm)

-- help box
helpBox :: Widget Name
helpBox =
  str $
    "Keys:\n"
      <> "Ctrl+S: search\n"
      <> "Ctrl+N: switch to next panel\n"
      <> "Ctrl+P: switch to previous panel\n"
      <> "Esc:    quit"

-- result box
resultBox :: AppState -> Widget Name
resultBox st = renderList listDrawResult True $ list ResultRegion l 1
  where
    -- according to AppState, do `searchCron`
    l = Vec.fromList $ st ^. searchedResult

-- info box
infoBox :: AppState -> Widget Name
infoBox st =
  case (st ^. searchedResult) !? (st ^. selectedResult) of
    Nothing -> emptyWidget
    Just cs -> renderList listDrawInfo False $ list DetailRegion (l cs) 1
  where
    l :: CronSchema -> Vec.Vector String
    -- TODO: max display length
    l = Vec.fromList . flip genCronStrings resultBoxColumns

-- safe `!!`
(!?) :: [a] -> Int -> Maybe a
{-# INLINEABLE (!?) #-}
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n

genCronStrings :: CronSchema -> [String] -> [String]
genCronStrings cs c = [c' <> ": " <> s' | (c', s') <- zip c (getCronStrings cs c)]

----------------------------------------------------------------------------------------------------

mkForm :: Search -> Form Search e Name
mkForm =
  newForm
    [ labelP "Lookup string" @@= editTextField searchString (SearchRegion StringField) (Just 1),
      label "Select columns" @@= checkboxField selectInputCol (SearchRegion SelectInputField) "Input",
      label "" @@= checkboxField selectCmdCol (SearchRegion SelectCmdField) "Cmd",
      labelP "" @@= checkboxField selectOutputCol (SearchRegion SelectOutputField) "Output",
      labelP "Conjunction" @@= radioField conjunction radioG,
      labelP "Case sensitive" @@= checkboxField caseSensitive (SearchRegion CaseSensitiveField) ""
    ]
  where
    label s w = vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> hLimit 100 w
    labelP s w = padBottom (Pad 1) $ label s w
    radioG = [(AND, SearchRegion ConjAndField, "And"), (OR, SearchRegion ConjOrField, "Or")]

-- draw an item in `[CronSchema]` list
listDrawResult :: Bool -> CronSchema -> Widget Name
listDrawResult sel cs =
  let ws = if sel then s else u
   in hLimitPercent 100 $ hBox $ alignColumns columnAlignments columnWidths ws
  where
    -- TODO: c is `[String]`, attrName cannot cover list
    c = getCronStrings cs resultBoxColumns
    s = withAttr resultSelectedListAttr . str <$> c
    u = withAttr resultListAttr . str <$> c

-- ["idx", "dag", "name", "sleeper", "input", "cmd", "output", "activate", "file"]
columnWidths :: [Int]
columnWidths = [5, 10, 15, 10, 20, 20, 20, 5, 20]

columnAlignments :: [ColumnAlignment]
columnAlignments = replicate (length resultBoxColumns) AlignLeft

listDrawInfo :: Bool -> String -> Widget Name
listDrawInfo _ = str

----------------------------------------------------------------------------------------------------
-- Event
----------------------------------------------------------------------------------------------------

appEvent :: BrickEvent Name () -> EventM Name AppState ()
-- quit
appEvent (VtyEvent (V.EvResize {})) = return ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
-- switch between `Name`
appEvent (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just (SearchRegion _) -> focusRing %= F.focusSetCurrent ResultRegion
    Just ResultRegion -> focusRing %= F.focusSetCurrent (SearchRegion StringField)
    _ -> return ()
appEvent (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just (SearchRegion _) -> focusRing %= F.focusSetCurrent ResultRegion
    Just ResultRegion -> focusRing %= F.focusSetCurrent (SearchRegion StringField)
    _ -> return ()
-- press Enter to search while in `SearchRegion`
appEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) =
  -- TODO: state change?
  modify commitSearchRequest
-- other cases
appEvent ev@(VtyEvent ve) = do
  r <- use focusRing
  case F.focusGetCurrent r of
    -- handle `SearchRegion`
    Just (SearchRegion _) ->
      -- TODO: not working?
      zoom searchForm $ handleFormEvent ev
    -- handle `ResultRegion`
    Just ResultRegion ->
      -- TODO: not working?
      zoom searchedResultList $ handleListEvent ve
    _ -> return ()
appEvent _ = return ()

-- according to the current form states, update filtered result
commitSearchRequest :: AppState -> AppState
commitSearchRequest st =
  let flt = [st ^. search . selectInputCol, st ^. search . selectCmdCol, st ^. search . selectOutputCol]
      cols = ["input", "cmd", "output"]
      sf = [c | (c, f) <- zip cols flt, f]
      sp = SearchParam sf (st ^. search . conjunction) (T.unpack $ st ^. search . searchString)
      sr = searchCron sp (st ^. allCrons)
   in st & searchedResult .~ sr & searchedResultList .~ list ResultRegion (Vec.fromList sr) 1

----------------------------------------------------------------------------------------------------
-- Attr
----------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.black),
      (editFocusedAttr, V.black `on` V.yellow),
      -- (listAttr, V.white `on` V.blue),
      (resultListAttr, V.white `on` V.blue),
      (resultSelectedListAttr, V.blue `on` V.white),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

resultListAttr :: AttrName
resultListAttr = listAttr <> attrName "resultList"

resultSelectedListAttr :: AttrName
resultSelectedListAttr = listSelectedAttr <> attrName "resultSelectedList"

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
      _selectInputCol = True,
      _selectCmdCol = True,
      _selectOutputCol = True,
      _conjunction = AND,
      _caseSensitive = False
    }

focusRingList :: [Name]
focusRingList =
  [ SearchRegion StringField,
    SearchRegion SelectInputField,
    SearchRegion SelectCmdField,
    SearchRegion SelectOutputField,
    SearchRegion ConjAndField,
    SearchRegion ConjOrField,
    SearchRegion CaseSensitiveField,
    ResultRegion
  ]

initialState :: [CronSchema] -> AppState
initialState cs =
  AppState
    { _focusRing = F.focusRing focusRingList,
      _search = defaultSearch,
      _searchForm = mkForm defaultSearch,
      _allCrons = cs,
      _searchedResult = [],
      _searchedResultList = list ResultRegion Vec.empty 0,
      _selectedResult = 0
    }

appCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

----------------------------------------------------------------------------------------------------
-- Conf
----------------------------------------------------------------------------------------------------

data CronSettings where
  CronSettings :: {lookupDirs :: [String], version :: Float} -> CronSettings
  deriving (Show, Generic)

instance FromJSON CronSettings

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
  let vtyBuilder = do
        v <- mkVty V.defaultConfig
        -- TODO: not working
        -- enable Mouse control
        V.setMode (V.outputIface v) V.Mouse True
        return v
  initialVty <- vtyBuilder

  -- Tui
  void $ customMain initialVty vtyBuilder Nothing app (initialState crons)
