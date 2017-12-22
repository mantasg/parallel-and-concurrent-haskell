-- Merging

module Concurrent.Chapter8.Section4 where

import Concurrent.Chapter8.Section3
import Control.Concurrent
import Control.Monad
import Control.Exception
import Utils

vals = [1,2,3,4,5]

-- All the calls feed into the same MVar which is a bit inconvenient
waitForFirst :: IO ()
waitForFirst = do
  m <- newEmptyMVar
  let runp v = waitAndReturn v v >>= putMVar m
  mapM_ (forkIO . runp) vals
  takeMVar m >>= print
  replicateM_ (length vals - 1) (takeMVar m)

waitEither :: Async a -> Async b -> IO (Async (Either a b))
waitEither a b = do 
  m <- newEmptyMVar 
  forkIO $ try (Left  <$> wait a) >>= putMVar m
  forkIO $ try (Right <$> wait b) >>= putMVar m
  return (Async m)

testWaitEither :: IO ()
testWaitEither = do 
  a1 <- async $ waitAndReturn 3 "Foo"
  a2 <- async $ waitAndReturn 2 "Bar"
  a3 <- waitEither a1 a2
  wait a3 >>= print

waitAny :: Show a => [Async a] -> IO a
waitAny as = do 
  m <- newEmptyMVar
  forM_ as $ \a -> forkIO (wait a >>= putMVar m)
  takeMVar m

testWaitAny :: IO ()
testWaitAny = do 
  as <- mapM async 
    [ waitAndReturn 10 "a"
    , waitAndReturn 2 "b"
    , waitAndReturn 3 "c" 
    ]
  waitAny as >>= print