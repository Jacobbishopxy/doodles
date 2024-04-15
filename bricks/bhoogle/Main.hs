{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- file: Main.hs
-- author: Jacob Xie
-- date: 2024/03/19 17:07:00 Tuesday
-- brief:

module Main where

import Brick
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Concurrent
import Control.Lens
import Data.List qualified as Lst
import Data.Text qualified as Txt
import Data.Time qualified as Tm
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Input.Events qualified as K
import Hoogle qualified as H
import Protolude

--
newtype Event = EventUpdateTime Tm.LocalTime

-- controls
data Name
  = TypeSearch
  | TextSearch
  | ListResults
  deriving (Show, Eq, Ord)

data SortBy
  = SortNone
  | SortAsc
  | SortDec
  deriving (Eq)

data BrickState = BrickState
  { _stEditType :: !(BE.Editor Text Name), -- the type to search for
    _stEditText :: !(BE.Editor Text Name), -- a text search in the results
    _stTime :: !Tm.LocalTime, -- current time
    _stFocus :: !(BF.FocusRing Name), -- a circular list of focusable controls
    _stResults :: [H.Target], -- the last set of search results from Hoogle
    _stResultsList :: !(BL.List Name H.Target), -- list for the search results
    _stSortResults :: SortBy -- current sort order for the results
  }

makeLenses ''BrickState

app :: App BrickState Event Name
app =
  App
    { appDraw = drawUi,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pass,
      appAttrMap = const theMap
    }

handleEvent :: BrickEvent Name Event -> EventM Name BrickState ()
handleEvent ev =
  case ev of
    -- handle keyboard events; k is the key, ms are the modifier keys
    (VtyEvent ve@(V.EvKey k ms)) ->
      --
      case (k, ms) of
        -- Escape quits the app, no matter what control has focus
        (K.KEsc, []) -> halt
        _ -> do
          st' <- get
          -- based on focused control
          case BF.focusGetCurrent $ st' ^. stFocus of
            Just TypeSearch ->
              case k of
                -- search, clear sort order, focus next
                K.KChar '\t' -> do
                  found <- liftIO $ doSearch st'
                  modify $ \st -> filterResults $ st & stFocus %~ BF.focusNext & stResults .~ found & stSortResults .~ SortNone
                -- Search, clear sort order, focus prev
                K.KBackTab -> do
                  found <- liftIO $ doSearch st'
                  modify $ \st -> filterResults $ st & stFocus %~ BF.focusPrev & stResults .~ found & stSortResults .~ SortNone
                -- Search, clear sort order, focus prev
                K.KEnter -> do
                  found <- liftIO $ doSearch st'
                  modify $ \st -> filterResults $ st & stResults .~ found & stSortResults .~ SortNone & stFocus %~ BF.focusSetCurrent ListResults
                -- let the editor handle all other events
                _ -> do
                  Brick.zoom stEditType $ BE.handleEditorEvent ev
                  st <- get
                  st2 <- liftIO $ searchAhead doSearch st
                  put st2
            Just TextSearch ->
              case k of
                -- focus next
                K.KChar '\t' -> modify $ \st -> st & stFocus %~ BF.focusNext
                -- focus previous
                K.KBackTab -> modify $ \st -> st & stFocus %~ BF.focusPrev
                -- let the editor handle all other events
                _ -> do
                  Brick.zoom stEditText $ BE.handleEditorEvent ev
                  modify filterResults
            Just ListResults ->
              case k of
                -- focus next
                K.KChar '\t' -> modify $ \st -> st & stFocus %~ BF.focusNext
                -- focus previous
                K.KBackTab -> modify $ \st -> st & stFocus %~ BF.focusPrev
                -- toggle the search order between ascending and descending, use asc if sort order was 'none'
                K.KChar 's' ->
                  let sortDir = if (st' ^. stSortResults) == SortAsc then SortDec else SortAsc
                   in let sorter =
                            if sortDir == SortDec
                              then Lst.sortBy (flip compareType)
                              else Lst.sortBy compareType
                       in modify $ \st -> filterResults $ st & stResults %~ sorter & stSortResults .~ sortDir
                -- let the editor handle all other events
                -- using handleListEventVi which adds vi-style keybindings for navigation
                -- and the standard handleListEvent as a fallback for all other events
                _ -> do
                  Brick.zoom stResultsList $ BL.handleListEventVi BL.handleListEvent ve
            _ -> pass
    -- update the time in the state
    (AppEvent (EventUpdateTime time)) ->
      modify $ \st -> st & stTime .~ time
    _ -> pass
  where
    doSearch :: BrickState -> IO [H.Target]
    doSearch st' = liftIO $ searchHoogle $ Txt.strip . Txt.concat $ BE.getEditContents $ st' ^. stEditType

-- search ahead for type strings longer than 3 chars
searchAhead :: (BrickState -> IO [H.Target]) -> BrickState -> IO BrickState
searchAhead search st =
  let searchText = Txt.strip . Txt.concat . BE.getEditContents $ st ^. stEditType
   in if Txt.length searchText > 3
        then do
          -- search
          found <- search st
          pure . filterResults $ st & stResults .~ found & stSortResults .~ SortNone
        else
          -- just clear
          pure $ st & stResults .~ [] & stResultsList %~ BL.listClear

-- filter the results from Hoogle using the search text
filterResults :: BrickState -> BrickState
filterResults st =
  let allResults = st ^. stResults
   in let filterText = Txt.toLower . Txt.strip . Txt.concat . BE.getEditContents $ st ^. stEditText
       in let results =
                if Txt.null filterText
                  then allResults
                  else filter (Txt.isInfixOf filterText . Txt.toLower . formatResult) allResults
           in st & stResultsList .~ BL.list ListResults (Vec.fromList results) 1

drawUi :: BrickState -> [Widget Name]
drawUi st = [padAll 1 contentBlock]
  where
    contentBlock = withBorderStyle BBS.unicode (BB.border searchBlock) <=> padTop (Pad 1) resultsBlock
    resultsBlock =
      let total = show . length $ st ^. stResults
       in let showing = show . length $ st ^. (stResultsList . BL.listElementsL)
           in withAttr (attrName "infoTitle") $ txt "Results: " <+> txt (showing <> "/" <> total) <=> padTop (Pad 1) (resultsContent <+> resultsDetail)
    resultsContent = BL.renderList (\_ e -> txt $ formatResult e) False (st ^. stResultsList)
    resultsDetail =
      padLeft (Pad 1) $
        hLimit 60 $
          vtitle "package:"
            <=> padLeft (Pad 2) (txt $ getSelectedDetail $ maybe "" (Txt.pack . fst) . H.targetPackage)
            <=> vtitle "module:"
            <=> padLeft (Pad 2) (txt $ getSelectedDetail $ maybe "" (Txt.pack . fst) . H.targetModule)
            <=> vtitle "docs:"
            <=> padLeft (Pad 2) (txt $ getSelectedDetail $ Txt.pack . clean . H.targetDocs)
            <=> fill ' '
    searchBlock =
      ((htitle "Type: " <+> editor TypeSearch (st ^. stEditType)) <+> time (st ^. stTime))
        <=> (htitle "Text: " <+> editor TextSearch (st ^. stEditText))
    htitle t = hLimit 20 $ withAttr (attrName "infoTitle") $ txt t
    vtitle t = withAttr (attrName "infoTitle") $ txt t
    editor n e = vLimit 1 $ BE.renderEditor (txt . Txt.unlines) (BF.focusGetCurrent (st ^. stFocus) == Just n) e
    time t = padLeft (Pad 1) $ hLimit 20 $ withAttr (attrName "time") $ str (Tm.formatTime Tm.defaultTimeLocale "%H-%M-%S" t)
    getSelectedDetail fn =
      case BL.listSelectedElement $ st ^. stResultsList of
        Nothing -> ""
        Just (_, e) -> fn e

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (BE.editAttr, V.black `Brick.on` V.cyan),
      (BE.editFocusedAttr, V.black `Brick.on` V.yellow),
      (BL.listAttr, V.white `Brick.on` V.blue),
      (BL.listSelectedAttr, V.blue `Brick.on` V.white),
      (attrName "infoTitle", fg V.cyan),
      (attrName "time", fg V.yellow)
    ]

-- compare two Hoogle results for sorting
compareType :: H.Target -> H.Target -> Ordering
compareType a b = compare (formatResult a) (formatResult b)

-- search Hoogle using the default Hoogle database
searchHoogle :: Text -> IO [H.Target]
searchHoogle f = do
  d <- H.defaultDatabaseLocation
  H.withDatabase d (\x -> pure $ H.searchDatabase x (Txt.unpack f))

-- format the Hoogle results so they roughly match what the terminal app would show
formatResult :: H.Target -> Text
formatResult t =
  let typ = clean $ H.targetItem t
   in let m = clean . fst <$> H.targetModule t
       in Txt.pack $ fromMaybe "" m <> " :: " <> typ

clean :: [Char] -> [Char]
clean = unescapeHTML . stripTags

-- from Hoogle source
unescapeHTML :: [Char] -> [Char]
unescapeHTML ('&' : xs)
  | Just x <- Lst.stripPrefix "lt;" xs = '<' : unescapeHTML x
  | Just x <- Lst.stripPrefix "gt;" xs = '>' : unescapeHTML x
  | Just x <- Lst.stripPrefix "amp;" xs = '&' : unescapeHTML x
  | Just x <- Lst.stripPrefix "quot;" xs = '\"' : unescapeHTML x
unescapeHTML (x : xs) = x : unescapeHTML xs
unescapeHTML [] = []

-- From Hakyll source
stripTags :: [Char] -> [Char]
stripTags [] = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs) = x : stripTags xs

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  -- create a bounded channel for events
  chan <- BCh.newBChan 5

  -- send a tick event every 1 seconds with the current time
  -- Brick will send this to our event handler which can then update the stTime field
  void . forkIO $ forever $ do
    t <- getTime
    BCh.writeBChan chan $ EventUpdateTime t
    threadDelay $ 1 * 1_000_000

  -- initial current time value
  t <- getTime

  -- construct the initial state values
  let st =
        BrickState
          { _stEditType = BE.editor TypeSearch (Just 1) "",
            _stEditText = BE.editor TextSearch (Just 1) "",
            _stResultsList = BL.list ListResults Vec.empty 1,
            _stTime = t,
            _stFocus = BF.focusRing [TypeSearch, TextSearch, ListResults],
            _stResults = [],
            _stSortResults = SortNone
          }

  -- run Brick (`mkVty` now available in `vty-crossplatform` package)
  let vtyBuilder = mkVty V.defaultConfig
  initialVty <- vtyBuilder

  void $ customMain initialVty vtyBuilder (Just chan) app st
  where
    getTime = do
      t <- Tm.getCurrentTime
      tz <- Tm.getCurrentTimeZone
      pure $ Tm.utcToLocalTime tz t
