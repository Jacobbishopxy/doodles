-- file: TM.hs
-- author: Jacob Xie
-- date: 2024/07/02 08:40:31 Tuesday
-- brief:

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import OpsLib.ThreadManager

testCase1_fork_and_check :: IO ()
testCase1_fork_and_check = do
  mgr <- newManager

  -- Fork some threads
  tid1 <- forkManaged mgr (do threadDelay 1000000; putStrLn "Thread 1 done")
  tid2 <- forkManaged mgr (do threadDelay 2000000; putStrLn "Thread 2 done")
  tid3 <- forkManaged mgr (do threadDelay 3000000; putStrLn "Thread 3 done")

  -- Check initial statuses
  st1 <- getStatus mgr tid1
  print ("Initial status of tid1: ", st1)
  st2 <- getStatus mgr tid2
  print ("Initial status of tid1: ", st2)
  st3 <- getStatus mgr tid3
  print ("Initial status of tid1: ", st3)

  -- Wait for a specific thread to finish
  fs1 <- waitFor mgr tid1
  print ("Final status of tid1: ", fs1)

  -- Wait for all threads to finish
  putStrLn "Wait all done..."
  waitAll mgr
  fs2 <- getStatus mgr tid2
  fs3 <- getStatus mgr tid3
  print ("Final status of tid2: ", fs2)
  print ("Final status of tid3: ", fs3)

  -- Demonstrating exception handling
  tid4 <- forkManaged mgr (do threadDelay 1000000; throwIO $ userError "This thread throws an exception")
  fs4 <- waitFor mgr tid4
  print ("Final status of tid4: ", fs4)

  return ()

testCase2_kill_a_thread :: IO ()
testCase2_kill_a_thread = do
  mgr <- newManager

  -- Fork a long-running thread
  tid1 <- forkManaged mgr (do threadDelay 5000000; putStrLn "This should not be printed if the thread is killed")

  -- Check initial status
  st1 <- getStatus mgr tid1
  print ("Status of tid1: ", st1)

  -- Kill the thread before it completes
  ks <- killManaged mgr tid1
  print ("Kill status of tid1: ", ks)

  -- Check final status
  fs <- getStatus mgr tid1
  print ("Final status of tid1: ", fs)

  return ()

main :: IO ()
main = do
  testCase1_fork_and_check
  testCase2_kill_a_thread

  putStrLn "done."
