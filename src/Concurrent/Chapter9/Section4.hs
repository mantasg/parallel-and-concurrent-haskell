module Concurrent.Chapter9.Section4 where

import Control.Concurrent hiding (Chan, readMVar)
import Control.Exception

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return $ Chan readVar writeVar

readChan :: Chan a -> IO a
readChan (Chan r _) = do
  modifyMVar r $ \readStream -> do
    Item v tail <- readMVar readStream
    return (tail, v)

readMVar :: MVar a -> IO a
readMVar m = mask_ $ do  -- mask_ is like mask but without taking in restore 
  v <- takeMVar m
  putMVar m v
  return v

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ w) a = do 
  newHole <- newEmptyMVar
  mask_ $ do
    oldHole <- takeMVar w
    putMVar oldHole (Item a newHole)
    putMVar w newHole
