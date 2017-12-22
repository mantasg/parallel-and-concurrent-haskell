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

-- Something's wrong with this example
problem2 :: MVar a -> (a -> IO a) -> IO ()
problem2 m f = mask $ \restore -> do 
  a <- takeMVar m
  threadDelay 2000000
  a' <- restore (f a) `catch` \e -> putMVar m a >> throw (e :: SomeException)
  putMVar m a'

testProblem :: IO ()
testProblem = do 
  m <- newMVar ()
  t <- forkIO $ problem2 m (return . id)
  forkIO $ threadDelay 1000000 >> throwTo t ThreadKilled
  void $ takeMVar m