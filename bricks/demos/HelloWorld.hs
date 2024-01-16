-- file: HelloWorld.hs
-- author: Jacob Xie
-- date: 2024/01/15 15:59:08 Monday
-- brief:

module Main where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
