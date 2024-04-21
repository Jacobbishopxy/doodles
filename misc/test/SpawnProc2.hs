-- file: SpawnProc2.hs
-- author: Jacob Xie
-- date: 2024/04/15 09:27:32 Monday
-- brief: https://gist.github.com/erantapaa/ebbcd56d1bccf3e57c75

module Main where

import Control.Concurrent
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)

cmdCompute :: String
cmdCompute = "./scripts/compute"

-- blocking IO
f1 :: String -> Int -> IO ExitCode
f1 cmd _ = do
  (Just inp, Just otp, _, pHandle) <- createProcess (proc cmdCompute []) {std_out = CreatePipe, std_in = CreatePipe}

  hSetBuffering inp NoBuffering
  hPutStrLn inp cmd -- send a command

  -- block until the response is received
  contents <- hGetLine otp
  putStrLn $ "got: " <> contents

  hClose inp -- and close the pipe
  putStrLn "waiting for process to terminate"
  waitForProcess pHandle

-- non-blocking IO, send one line, wait the timeout period for a response
f2 :: String -> Int -> IO ExitCode
f2 cmd tMicros = do
  (Just inp, Just otp, _, pHandle) <- createProcess (proc cmdCompute []) {std_out = CreatePipe, std_in = CreatePipe}

  hSetBuffering inp NoBuffering
  hPutStrLn inp cmd -- send a command, will respond after 4 seconds
  mVar <- newEmptyMVar
  tid <- forkIO $ hGetLine otp >>= putMVar mVar

  -- wait the timeout period for the response
  result <- timeout tMicros $ takeMVar mVar
  killThread tid

  case result of
    Nothing -> putStrLn "timed out"
    Just x -> putStrLn $ "got: " <> x

  hClose inp -- and close the pipe
  putStrLn "waiting for process to terminate"
  waitForProcess pHandle

-- non-blocking IO, send one line, report progress every timeout period
f3 :: String -> Int -> IO ExitCode
f3 cmd tMicors = do
  (Just inp, Just otp, _, pHandle) <- createProcess (proc cmdCompute []) {std_out = CreatePipe, std_in = CreatePipe}

  hSetBuffering inp NoBuffering
  hPutStrLn inp cmd -- send command
  mVar <- newEmptyMVar
  tid <- forkIO $ hGetLine otp >>= putMVar mVar

  -- loop until response received; report progress every timeout period
  let loop = do
        result <- timeout tMicors $ takeMVar mVar
        case result of
          Nothing -> putStrLn "still waiting..." >> loop
          Just x -> return x

  x <- loop
  killThread tid

  putStrLn $ "got: " <> x

  hClose inp -- and close the pipe
  putStrLn "waiting for process to terminate"
  waitForProcess pHandle

{-
Usage: ./prog which delay timeout
  where
    which   = main routine to run: 1, 2 or 3
    delay   = delay in seconds to send to compute script
    timeout = timeout in seconds to wait for response
E.g.:
  cabal run spawn-proc2 1 4 3   -- note: timeout is ignored for main1
  cabal run spawn-proc2 2 2 3   -- should get response
  cabal run spawn-proc2 2 4 3   -- should timeout
  cabal run spawn-proc2 3 4 1   -- should see "still waiting..." a couple of times
-}

main :: IO ExitCode
main = do
  (which : vTime : tout : _) <- map read <$> getArgs
  let cmd = "10 " ++ show vTime
      tMicros = 1000000 * tout :: Int
  case which of
    1 -> f1 cmd tMicros
    2 -> f2 cmd tMicros
    3 -> f3 cmd tMicros
    _ -> error "huh?"
