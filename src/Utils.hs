module Utils where

import Control.Concurrent

delay :: Int -> IO ()
delay = threadDelay . (*1000000)

waitAndReturn :: Int -> a -> IO a
waitAndReturn s a = delay s >> return a  
