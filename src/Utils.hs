module Utils where

import Control.Concurrent

waitAndReturn :: Int -> a -> IO a
waitAndReturn s a = threadDelay (s * 1000000) >> return a  
