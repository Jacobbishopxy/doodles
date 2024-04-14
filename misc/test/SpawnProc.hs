-- file: SpawnProc.hs
-- author: Jacob Xie
-- date: 2024/04/14 22:33:19 Sunday
-- brief:

module Main where

import Control.Concurrent (threadDelay)
import System.IO
import System.Process

-- blocking action
f1 :: IO ()
f1 = do
  (_, Just hout, _, _) <- createProcess (proc "ls" ["-l"]) {std_out = CreatePipe}
  output <- hGetContents hout
  putStrLn output

f2 :: IO ()
f2 = do
  (_, _, _, _) <- createProcess (proc "ls" ["-l"]) {std_out = NoStream, std_err = NoStream}
  putStrLn "Command started, but may still be running in the background..."
  threadDelay 2_000_000
  putStrLn "Done sleeping, the command may still be running..."

main :: IO ()
main = do
  f1
  f2
