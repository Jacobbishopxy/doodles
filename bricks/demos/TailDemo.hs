{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: TailDemo.hs
-- author: Jacob Xie
-- date: 2024/03/11 17:16:16 Monday
-- brief:

module Main where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (makeLenses)
import System.Random (randomRIO)

data AppState = AppState
  { _textAreaHeight :: Int,
    _textAreaWidth :: Int,
    _textAreaContents :: [T.Text]
  }

makeLenses ''AppState

draw :: AppState -> Widget n
draw st = header st <=> box st

header :: AppState -> Widget n
header st =
  padBottom (Pad 1) $
    hBox
      [ padRight (Pad 7) $
          str ("Max width: " <> show (_textAreaWidth st))
            <=> str "Left(-)/Right(+)",
        str ("Max height: " <> show (_textAreaHeight st))
          <=> str "Down(-)/Up(+)"
      ]

box :: AppState -> Widget n
box st =
  border $
    hLimit (_textAreaWidth st) $
      vLimit (_textAreaHeight st) $
        renderBottomUp (txtWrap <$> _textAreaContents st)

{-
  Given a list of widgets, draw them bottom-up in a vertical arrangement,
  i.e., the first widget in this list will appear at the bottom of the rendering area.
  Rendering stops when the rendering area is full, i.e., items that cannot be rendered
  are never evaluated or drawn.
-}
renderBottomUp :: [Widget n] -> Widget n
renderBottomUp ws =
  Widget Greedy Greedy $ do
    let go _ [] = return V.emptyImage
        go remainingHeight (c : cs) = do
          cResult <- render c
          let img = image cResult
              newRemainingHeight = remainingHeight - V.imageHeight img
          if newRemainingHeight == 0
            then return img
            else
              if newRemainingHeight < 0
                then return $ V.cropTop remainingHeight img
                else do
                  rest <- go newRemainingHeight cs
                  return $ V.vertCat [rest, img]
    ctx <- getContext
    img <- go (availHeight ctx) ws
    render $ fill ' ' <=> raw img

textLines :: [T.Text]
textLines =
  [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut",
    "labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco",
    "laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit",
    "in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat",
    "cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  ]

handleEvent :: BrickEvent n CustomEvent -> EventM n AppState ()
handleEvent (AppEvent (NewLine l)) = textAreaContents %= (l :)
handleEvent (VtyEvent (V.EvKey V.KUp [])) = textAreaHeight %= (+ 1)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = textAreaHeight %= max 0 . subtract 1
handleEvent (VtyEvent (V.EvKey V.KRight [])) = textAreaWidth %= (+ 1)
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = textAreaWidth %= max 0 . subtract 1
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ = return ()

newtype CustomEvent = NewLine T.Text

app :: App AppState CustomEvent ()
app =
  App
    { appDraw = (: []) . draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appAttrMap = const $ attrMap V.defAttr [],
      appStartEvent = return ()
    }

initialState :: AppState
initialState =
  AppState
    { _textAreaHeight = 20,
      _textAreaWidth = 40,
      _textAreaContents = []
    }

{-
  Run forever, generating new lines of text for the application window,
  prefixed with a line number. This function simulates the kind of behavior that
  you'd get from running 'tail -f' on a file.
-}
generateLines :: BChan CustomEvent -> IO ()
generateLines chan = go (1 :: Integer)
  where
    go lineNum = do
      -- Wait a random amount of time (in milliseconds)
      let delayOptions = [500, 1000, 2000]
      delay <- randomVal delayOptions
      threadDelay $ delay * 1000

      -- Choose a random line of text from our collection
      l <- randomVal textLines

      -- Send it to the application to be added to the UI
      writeBChan chan $ NewLine $ T.pack ("Line " <> show lineNum <> " - ") <> l

      go $ lineNum + 1

randomVal :: [a] -> IO a
randomVal as = do
  idx <- randomRIO (0, length as - 1)
  return $ as !! idx

main :: IO ()
main = do
  chan <- newBChan 10

  -- Run thread to simulate incoming data
  void $ forkIO $ generateLines chan

  void $ customMainWithDefaultVty (Just chan) app initialState
