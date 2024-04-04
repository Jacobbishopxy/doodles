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
  ( checkboxField,
    editTextField,
    newForm,
    radioField,
    renderForm,
    (@@=),
  )
import Brick.Widgets.Border
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
import Lens.Micro.TH (makeLenses)
import MiscLib.CronSchema
import System.Environment (getArgs)

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

-- Source Name
data Name
  = FormRegion
  | ResultRegion
  | DetailRegion
  deriving (Eq, Ord, Show)

data ColsField = InputCol | CmdCol | OutputCol
  deriving (Show, Ord, Eq)

data ConjField = ConjAnd | ConjOr
  deriving (Show, Ord, Eq)

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    -- string to search
    _searchString :: T.Text,
    -- columns to search
    _selectInputCol :: Bool,
    _selectCmdCol :: Bool,
    _selectOutputCol :: Bool,
    -- conjunction
    _conj :: Conj,
    -- ignore case
    _caseSensitive :: Bool,
    -- all crons
    _allCrons :: [CronSchema],
    -- searched result
    _searchedResult :: [CronSchema],
    -- searched result
    _selectedResult :: Int
  }
  deriving (Show)

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- Const
----------------------------------------------------------------------------------------------------

resultBoxColumns :: [String]
resultBoxColumns = ["idx", "dag", "task", "sleeper", "input", "cmd", "output", "activate", "file"]

----------------------------------------------------------------------------------------------------
-- UI
----------------------------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st = [ui]
  where
    ui =
      vBox
        [ vLimitPercent 30 $
            hBox
              [ hLimitPercent 50 $ borderWithLabel titleSP $ controlBox st <+> fill ' ',
                borderWithLabel titleDI $ infoBox st <+> fill ' '
              ],
          borderWithLabel titleMR $ resultBox st
        ]
    titleSP = str "search param"
    titleDI = str "detailed info"
    titleMR = str "matched result"

-- control box
controlBox :: AppState -> Widget Name
controlBox =
  renderForm
    . newForm
      [ labelP "String" @@= editTextField searchString FormRegion (Just 1),
        label "Select columns" @@= checkboxField selectInputCol FormRegion "Input",
        label "" @@= checkboxField selectCmdCol FormRegion "Cmd",
        labelP "" @@= checkboxField selectOutputCol FormRegion "Output",
        labelP "Conjunction" @@= radioField conj [(AND, FormRegion, "And"), (OR, FormRegion, "Or")],
        labelP "Case sensitive" @@= checkboxField caseSensitive FormRegion ""
      ]
  where
    label s w = vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    labelP s w = padBottom (Pad 1) $ label s w

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
    Just cs -> renderList listDrawInfo False $ list ResultRegion (l cs) 1
  where
    l :: CronSchema -> Vec.Vector String
    l = Vec.fromList . flip getCronStrings resultBoxColumns

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

----------------------------------------------------------------------------------------------------

-- draw an item in `[CronSchema]` list
listDrawResult :: Bool -> CronSchema -> Widget Name
listDrawResult sel cs =
  let ws = if sel then withAttr selectedResultAttr . str <$> c else str <$> c
   in hLimit 100 $ hBox $ alignColumns columnAlignments columnWidths ws
  where
    c = getCronStrings cs resultBoxColumns

selectedResultAttr :: AttrName
selectedResultAttr = attrName "selectedResult"

nullCellAttr :: AttrName
nullCellAttr = attrName "nullCell"

columnWidths :: [Int]
columnWidths = replicate (length resultBoxColumns) 10

columnAlignments :: [ColumnAlignment]
columnAlignments = replicate (length resultBoxColumns) AlignLeft

listDrawInfo :: Bool -> String -> Widget Name
listDrawInfo _ = str

----------------------------------------------------------------------------------------------------
-- Event
----------------------------------------------------------------------------------------------------

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent = undefined

-- according to the current form states, update filtered result
filterSelectedFields :: AppState -> AppState
filterSelectedFields st =
  let flt = [st ^. selectInputCol, st ^. selectCmdCol, st ^. selectOutputCol]
      cols = ["input", "cmd", "output"]
      sf = [c | (c, f) <- zip cols flt, f]
      sp = SearchParam sf (st ^. conj) (T.unpack $ st ^. searchString)
   in st & searchedResult .~ searchCron sp (st ^. allCrons)

----------------------------------------------------------------------------------------------------
-- Attr
----------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (attrName "button-input", V.white `on` V.cyan),
      (attrName "button-cmd", V.white `on` V.green),
      (attrName "button-output", V.white `on` V.blue)
    ]

----------------------------------------------------------------------------------------------------
-- App
----------------------------------------------------------------------------------------------------

app :: App AppState e Name
app =
  App
    { appDraw = drawUi,
      appChooseCursor = appCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

initialState :: [CronSchema] -> AppState
initialState cs =
  AppState
    { _focusRing = F.focusRing [FormRegion, ResultRegion],
      _searchString = "",
      _selectInputCol = True,
      _selectCmdCol = True,
      _selectOutputCol = True,
      _conj = AND,
      _caseSensitive = False,
      _allCrons = cs,
      _searchedResult = [],
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
  let yamlPath = case args of
        (x : _) -> x
        [] -> "./cron_settings.yml"
  p <- Y.decodeFileEither yamlPath
  let s = fromRight (error "check yaml if exists") p
  crons <- getAllCrons $ lookupDirs s

  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  -- Tui
  void $ customMain initialVty vtyBuilder Nothing app (initialState crons)
