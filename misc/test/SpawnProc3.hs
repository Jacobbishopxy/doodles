-- file: SpawnProc3.hs
-- author: Jacob Xie
-- date: 2024/05/16 17:38:53 Thursday
-- brief:

import Control.Concurrent
import System.IO
import System.Process

main :: IO ()
main = do
  let scriptCommand = "bash ./scripts/rand_print.sh -h 10 -l 1 -m 20"

  (Just hIn, Just hOut, _, processHandle) <-
    createProcess
      (shell scriptCommand)
        { std_in = CreatePipe,
          std_out = CreatePipe
        }

  -- Set both input and output handles to line buffering mode
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering

  -- Start a separate thread to continuously print messages from the script
  _ <- forkIO $ printMessages hOut

  -- Wait for the process to finish
  _ <- waitForProcess processHandle
  putStrLn "Process finished."

printMessages :: Handle -> IO ()
printMessages h = do
  -- Read a line from the handle
  line <- hGetLine h
  -- Print the line
  putStrLn line
  -- Continue reading lines recursively
  printMessages h
