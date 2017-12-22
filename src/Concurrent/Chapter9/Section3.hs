module Concurrent.Chapter9.Section3 where

import Control.Monad (void)
import Control.Exception hiding (bracket)
import Control.Concurrent
import Utils

bracket' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket' before after thing = do
  a <- before
  c <- thing a `onException` after a
  void $ after a
  return c

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing = uninterruptibleMask $ \restore -> do
  a <- before
  c <- restore (thing a) `onException` after a
  void $ after a
  return c

testBracket :: IO ()
testBracket = do
  l <- newEmptyMVar
  t <- forkIO $ bracket'
    (delay 1 >> newEmptyMVar)        -- before
    (\_ -> delay 1 >> putMVar l ())  -- after
    (\m -> delay 1 >> putMVar m ())  -- thing

  forkIO $ delay 2 >> print "Killing" >> throwTo t ThreadKilled
  takeMVar l
