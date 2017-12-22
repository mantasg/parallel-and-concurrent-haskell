-- Exceptions in Haskell

module Concurrent.Chapter8.Section2 where

import Control.Exception


data FooException = FooException String deriving Show
instance Exception FooException

data UnknownException = UnknownException deriving Show
instance Exception UnknownException

brokenCall :: IO ()
-- it's better to use throwIO in IO monad because it guarantees strict order of operations
brokenCall = throwIO $ FooException "oops"
-- brokenCall = throw $ FooException "oops"
-- brokenCall = throw UnknownException

handleBrokenCallWithTry :: IO ()
handleBrokenCallWithTry = do
  r <- try brokenCall
  case r of 
    Left (FooException err) -> print err
    Right val -> print "success"

catchFooException :: FooException -> IO ()
catchFooException (FooException err) = print err

catchSomeException :: SomeException -> IO ()
catchSomeException _ = print "unknown exception"

handleBrokenCallWithCatch :: IO ()
handleBrokenCallWithCatch = brokenCall `catch` catchFooException `catch` catchSomeException

-- Do something before re-throwing exception
handleBrokenCallWithOnExc :: IO ()
handleBrokenCallWithOnExc = brokenCall `onException` print "foo" 

