-- Cancelling Async

module Concurrent.Chapter9.Section1 where

import Control.Concurrent
import Control.Exception
import Utils

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkIO $ try action >>= putMVar m
  return $ Async t m

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ a) = readMVar a

wait :: Async a -> IO a
wait a = waitCatch a >>= either throwIO return

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

testCancel :: IO ()
testCancel = do
  a <- async $ waitAndReturn 5 "Foo"
  forkIO $ threadDelay 10000000 >> cancel a
  waitCatch a >>= print