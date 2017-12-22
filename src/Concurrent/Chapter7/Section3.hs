module Concurrent.Chapter7.Section3 where

import Control.Concurrent hiding (Chan, newChan, writeChan, readChan)
import Data.List (replicate)
import Control.Monad (replicateM)

-- MVar as building block for Unbounded Channel

--type Stream a = MVar (Item a)
data Item a = Item a (MVar (Item a))

data Chan a = Chan (MVar (MVar (Item a))) (MVar (MVar (Item a)))

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return $ Chan readVar writeVar

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ w) a = do 
  newHole <- newEmptyMVar
  oldHole <- takeMVar w
  putMVar oldHole (Item a newHole)
  putMVar w newHole

readChan :: Chan a -> IO a
readChan (Chan r _) = do
  readStream <- takeMVar r
  Item v tail <- readMVar readStream
  putMVar r tail
  return v 

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ w) = do 
  hole <- readMVar w
  readVar <- newMVar hole
  return $ Chan readVar w


subs :: Chan a -> String -> IO ()
subs c tid = loop
  where 
    loop = readChan c >> putStr tid >> loop

testChans :: IO ()
testChans = do 
  c <- newChan
  forkIO $ subs c "A"
  forkIO $ subs c "B"
  forkIO $ subs c "C"
  replicateM 10 $ writeChan c ()
  getLine
  return ()

{-
  Important distinction:
    - takeMVar takes value from MVar and doesn't put it back (good for implementing competition for value)  
    - readMVar reads value and puts it back (good for implementing broadcasting)
-}