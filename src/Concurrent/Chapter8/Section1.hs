module Concurrent.Chapter8.Section1 where

import Control.Concurrent

-- Overlapping Input/Output

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  forkIO $ do 
    v <- action
    putMVar m v
  return $ Async m

wait :: Async a -> IO a
wait (Async m) = readMVar m

testAsync :: IO ()
testAsync = do 
  a1 <- async $ threadDelay 5000000 >> return "Res1"
  a2 <- async $ threadDelay 2000000 >> return "Res2"
  r1 <- wait a1
  r2 <- wait a2
  print (r1, r2)