module NiceFork
  ( ThreadManager,
    newManager,
    forkManaged,
    getStatus,
    waitFor,
    waitAll,
  )
where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad
import qualified Data.Map as M

newtype ThreadManager
  = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving (Eq)

data ThreadStatus
  = Running
  | Finished
  | Threw SomeException
  deriving (Show)

-- | Create a new thread manager.
newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state $ either Threw (const Finished) result
    return (M.insert tid state m, tid)

-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m -> case M.lookup tid m of
    Nothing -> return (m, Nothing)
    Just st ->
      tryTakeMVar st
        >>= \mst -> return $ case mst of
          Nothing -> (m, Just Running)
          x@(Just _) -> (M.delete tid m, x)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid =
  -- Old waitFor:
  -- do
  -- maybeDone <- modifyMVar mgr $ \m ->
  --   return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
  --     (Nothing, _) -> (m, Nothing)
  --     (done, m') -> (m', done)

  -- case maybeDone of
  --   Nothing -> return Nothing
  --   Just st -> Just <$> takeMVar st

  -- New waitFor(2):
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just <$> takeMVar st)

-- | Block until all managed threads terminate.
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where
    elems m = return (M.empty, M.elems m)
