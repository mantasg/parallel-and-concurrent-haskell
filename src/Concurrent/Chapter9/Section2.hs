module Concurrent.Chapter9.Section2 where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)

problem1 :: MVar a -> (a -> IO a) -> IO ()
problem1 m f = do 
  a <- takeMVar m
  threadDelay 2000000
  a' <- f a `catch` \e -> putMVar m a >> throw (e :: SomeException)
  putMVar m a'

problem2 :: MVar a -> (a -> IO a) -> IO ()
-- mask can be interrupted by interruptable functions
-- uninterruptibleMask cannot be interrupted, however it should be used with caution
problem2 m f = uninterruptibleMask $ \restore -> do 
  a <- takeMVar m
  threadDelay 2000000
  a' <- restore (f a) `catch` \e -> putMVar m a >> throw (e :: SomeException)
  putMVar m a'

-- modifyMVar_ insulated from need to use mask
solution :: MVar a -> (a -> IO a) -> IO ()
solution = modifyMVar_ 

testProblem :: IO ()
testProblem = do 
  m <- newMVar 1
  t <- forkIO $ solution m (return . (const 2))
  forkIO $ threadDelay 1000000 >> throwTo t ThreadKilled
  takeMVar m >>= print