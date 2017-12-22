-- MVar as simple channel

module Concurrent.Chapter7.Section1 where

import           Control.Concurrent


data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do 
  m <- newEmptyMVar
  let l = Logger m 
  forkIO $ loop m
  return l  
  where 
    loop m = do
      cmd <- takeMVar m
      case cmd of 
        Message msg -> do
          print msg
          loop m
        Stop s -> do 
          print "Stopping logger"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) str = putMVar m (Message str)

stopLogger :: Logger -> IO ()
stopLogger (Logger m) = do 
  lock <- newEmptyMVar
  putMVar m $ Stop lock
  takeMVar lock

testLogger :: IO ()
testLogger = do 
  l <- initLogger
  logMessage l "foo"
  logMessage l "foo"
  logMessage l "foo"
  stopLogger l

-- The above Logger is not very good because MVar can only contain one value at the time and in case of heavy load it would block logging calls
testPutMVar :: IO ()
testPutMVar = do 
  v <- newEmptyMVar
  putMVar v ()
  print "Before put"
  forkIO $ getLine >> takeMVar v
  putMVar v ()
  print "After put"