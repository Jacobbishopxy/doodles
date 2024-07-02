-- file: ThreadManager.hs
-- author: Jacob Xie
-- date: 2024/07/01 13:37:42 Monday
-- brief:

module OpsLib.ThreadManager
  ( ThreadStatus (..),
    ThreadManager,
    newManager,
    forkManaged,
    killManaged,
    getStatus,
    waitFor,
    waitAll,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    forkIO,
    killThread,
    modifyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    tryTakeMVar,
  )
import Control.Exception (IOException, try)
import Control.Monad (join)
import qualified Data.Map as M

data ThreadStatus
  = Running
  | Finished
  | Killed
  | Threw IOException
  deriving (Eq, Show)

newtype ThreadManager
  = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving (Eq)

newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) fn =
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try fn
      putMVar state $ either Threw (const Finished) result
    return (M.insert tid state m, tid)

killManaged :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
killManaged (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> do
        killThread tid
        putMVar st Killed
        return (M.delete tid m, Just Killed)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> tryTakeMVar st >>= mst m
  where
    mst m' mm = case mm of
      Nothing -> return (m', Just Running)
      Just sth -> return (M.delete tid m', Just sth)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid =
  join . modifyMVar mgr $ \m -> return $
    case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just <$> takeMVar st)

waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr es >>= mapM_ takeMVar
  where
    es m = return (M.empty, M.elems m)
