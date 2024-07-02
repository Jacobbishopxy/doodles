-- file: StateThreadM.hs
-- author: Jacob Xie
-- date: 2024/07/01 15:17:54 Monday
-- brief:

module OpsLib.StateThreadM
  ( StateThreadM,
    newSManager,
  )
where

import Control.Concurrent (MVar, ThreadId, newMVar)
import Control.Monad.State
import qualified Data.Map as M
import OpsLib.ThreadManager (ThreadStatus)

newtype StateThreadM a
  = Mgr (MVar (M.Map ThreadId (MVar (State ThreadStatus a))))
  deriving (Eq)

newSManager :: IO (StateThreadM a)
newSManager = Mgr <$> newMVar M.empty
