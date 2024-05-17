-- file: SpawnProc3.hs
-- author: Jacob Xie
-- date: 2024/05/16 17:38:53 Thursday
-- brief:

import Control.Concurrent (Chan, forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (when)
import System.IO (Handle, hClose, hGetLine)
import System.Process (StdStream (CreatePipe), createProcess, proc, std_err, std_out, waitForProcess)

-- Function to read output from the handle and write it to the channel
enqueueOutput :: Handle -> Chan String -> IO ()
enqueueOutput hOut chan = do
  line <- hGetLine hOut
  writeChan chan line
  enqueueOutput hOut chan -- Continue reading

main :: IO ()
main = do
  putStrLn "main start"

  -- Start the subprocess
  (_, Just hOut, Just hErr, ph) <-
    createProcess
      (proc "/bin/bash" ["./scripts/rand_print.sh", "-h", "3", "-l", "1", "-m", "5"])
        { std_out = CreatePipe,
          std_err = CreatePipe
        }

  -- Create a new channel
  chan <- newChan

  -- Fork a new thread to read from the subprocess stdout
  _ <- forkIO $ enqueueOutput hOut chan

  -- Process the output from the channel
  let loop = do
        _l <- readChan chan
        putStrLn $ "_l: " ++ _l
        when ("Exiting..." `elem` words _l) $ return ()
        loop

  _ <- loop

  -- Wait for the subprocess to finish
  exitCode <- waitForProcess ph
  putStrLn $ "process finished with exit code: " ++ show exitCode

  -- Close the handles
  hClose hOut
  hClose hErr
