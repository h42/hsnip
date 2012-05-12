import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished         -- terminated normally
                  | Threw Exception  -- killed by uncaught exception
                    deriving (Eq, Show)

-- | Create a new thread manager.
newManager :: IO ThreadManager

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId

-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until all managed threads terminate.
waitAll :: ThreadManager -> IO ()

module NiceFork
    (
      ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

newManager = Mgr `fmap` newMVar M.empty

forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- forkIO $ do
        result <- try body
        putMVar state (either Threw (const Finished) result)
      return (M.insert tid state m, tid)

************************************************
import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (block, catch, throw, unblock)
import Prelude hiding (catch) -- use Control.Exception's version

modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io =
  block $ do
    a <- takeMVar m
    (b,r) <- unblock (io a) `catch` \e ->
             putMVar m a >> throw e
    putMVar m b
    return r
*************************************************



getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> tryTakeMVar st >>= \mst -> case mst of
                   Nothing -> return (m, Just Running)
                   Just sth -> return (M.delete tid m, Just sth)

waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, Nothing)
      (done, m') -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just `fmap` takeMVar st

waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)
