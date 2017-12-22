-- Async error propagation

module Concurrent.Chapter8.Section3 where 

import Control.Concurrent
import Control.Exception


-- In previous async example exceptions thrown in async exection would cause deadlocks since putMVar would never be reached. Hence the following example propagates the exception to caller as either result type (waitCatch) or rethrown exception (wait).

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async act = do 
  var <- newEmptyMVar
  forkIO $ try act >>= putMVar var
  return $ Async var

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async a) = readMVar a

wait :: Async a -> IO a
wait a = waitCatch a >>= either throwIO return