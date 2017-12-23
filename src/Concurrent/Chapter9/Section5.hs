{-# LANGUAGE DeriveAnyClass #-}

module Concurrent.Chapter9.Section5 where

import Control.Exception
import Control.Concurrent hiding (timeout)
import Data.Unique
import Utils

--- Trying out handleJust (catch with handled exception selection)

data Exception1 = Exception1 deriving (Show, Eq, Exception)
data Exception2 = Exception2 deriving (Show, Eq, Exception)

testHandleJust :: IO (Maybe ()) -> IO ()
testHandleJust act = handleJust 
        (\e -> if e == Exception1 then Just () else Nothing)
        (\e -> return Nothing)
        act >>= print

testHJ1 = testHandleJust (throwIO Exception1) -- returns Nothing (Exception1 was selected and handled)
testHJ2 = testHandleJust (throwIO Exception2) -- explodes with Exception2 (since it was not selected and therefore unhandled)
testHJ3 = testHandleJust (return (Just ()))   -- returns ()

---

instance Show Unique where show _ = "unique"
data Timeout = Timeout Unique deriving (Eq, Show)
instance Exception Timeout

timeout :: Int -> IO a -> IO (Maybe a)
timeout t m
  | t < 0  = fmap Just m 
  | t == 0 = return Nothing
  | otherwise = do
      pid <- myThreadId
      u <- newUnique
      let ex = Timeout u
      handleJust 
        (\e -> if e == ex then Just () else Nothing)
        (\_ -> return Nothing) $ 
        bracket (forkIO $ threadDelay (t * 1000000) >> throwTo pid ex) -- before
                (\tid -> throwTo tid ThreadKilled)         -- after
                (\_ -> fmap Just m)                        -- thing

testTimeout1 = timeout 2 $ delay 1 >> return () -- returns Just () after 1 second
testTimeout2 = timeout 2 $ delay 3 >> return () -- returns Nothing after 2 seconds 