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
    (@@=),
  )
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenterLayer, vCenterLayer)
import Brick.Widgets.Edit (Editor)
import Control.Monad (void)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import MiscLib.CronSchema
import MiscLib.CsvHelper

----------------------------------------------------------------------------------------------------
-- Adt
----------------------------------------------------------------------------------------------------

-- cols: str, string: str, conj: str, ignore_case: bool

-- Source Name
data Name
  = ColsField ColsField -- multi select
  | SearchField -- input
  | ConjField ConjField -- radio group
  | CaseSensitiveField -- check box
  | DisplayTable
  deriving (Eq, Ord, Show)

data ColsField = InputCol | CmdCol | OutputCol deriving (Show, Ord, Eq)

data ConjField = ConjAnd | ConjOr deriving (Show, Ord, Eq)

data AppState = AppState
  { _focusRing :: F.FocusRing Name,
    _lastReportedClick :: Maybe (Name, Location),
    -- string to search
    _searchString :: T.Text,
    -- columns to search
    _colsToSearch :: [ColsField],
    -- conjunction
    _conj :: Conj,
    -- ignore case
    _caseSensitive :: Bool,
    -- searched result
    _searchedResult :: [CronSchema]
  }

makeLenses ''AppState

----------------------------------------------------------------------------------------------------
-- Tui
----------------------------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st = undefined
  where
    -- label "Columns" @@=,
    form =
      newForm
        [ label "String" @@= editTextField searchString SearchField (Just 1),
          label "Select columns" @@= checkboxField selectInput (ColsField InputCol) "Input",
          label "" @@= checkboxField selectCmd (ColsField CmdCol) "Cmd",
          label "" @@= checkboxField selectOutput (ColsField OutputCol) "Output",
          label "Conjunction" @@= radioField conj [(AND, ConjField ConjAnd, "And"), (OR, ConjField ConjOr, "Or")],
          label "Case sensitive" @@= checkboxField caseSensitive CaseSensitiveField ""
        ]
    label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    -- Expected: (Bool -> f Bool) -> AppState -> f AppState
    selectInput = undefined
    selectCmd = undefined
    selectOutput = undefined

buttonLayer :: AppState -> Widget Name
buttonLayer st =
  vCenterLayer $
    hCenterLayer (padBottom (Pad 1) $ str "Select columns to be searched:")
      <=> hCenterLayer (hBox $ padLeftRight 1 <$> buttons)
  where
    buttons = mkButton <$> buttonData
    buttonData =
      [ (ColsField InputCol, "Input", attrName "button-input"),
        (ColsField CmdCol, "Cmd", attrName "button-cmd"),
        (ColsField OutputCol, "Output", attrName "button-output")
      ]
    mkButton (name, label, attr) =
      let wasClicked = (fst <$> st ^. lastReportedClick) == Just name
       in clickable name $
            withDefAttr attr $
              border $
                padTopBottom 1 $
                  padLeftRight (if wasClicked then 2 else 3) $
                    str (if wasClicked then "<" <> label <> ">" else label)

listDrawElement :: Bool -> CronSchema -> Widget Name
listDrawElement = undefined

----------------------------------------------------------------------------------------------------

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent = undefined

----------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (attrName "button-input", V.white `on` V.cyan),
      (attrName "button-cmd", V.white `on` V.green),
      (attrName "button-output", V.white `on` V.blue)
    ]

appCursor :: AppState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

----------------------------------------------------------------------------------------------------

initialState :: AppState
initialState = undefined

app :: App AppState e Name
app = undefined

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  -- Tui
  void $ customMain initialVty vtyBuilder Nothing app initialState
