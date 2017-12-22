-- MVar as container for shared state

module Concurrent.Chapter7.Section2 where

import           Control.Concurrent
import qualified Data.Map as M


type Phonebook = M.Map String String
newtype PhonebookState = PhonebookState (MVar Phonebook)

lookupPhone :: PhonebookState -> String -> IO (Maybe String)
lookupPhone (PhonebookState v) name = do 
  m <- takeMVar v
  putMVar v m
  return $ M.lookup name m

insertPhone :: PhonebookState -> String -> String -> IO () 
insertPhone (PhonebookState v) name num = do
  m <- takeMVar v
  -- The obvious way to do this would be as in the following line. However this could result in a lot of unevaluated thunks. The bright side of this is that we wouldn't be blocking for very long time. On the other hand it means that we could create a space leak.
  -- putMVar v (M.insert name num m)
  -- What we want is to not block for a very long time but also to evaluate as soon as we finish putMVar. This is achieved with following:
  let m' = M.insert name num m
  putMVar v m'
  seq m' (return ())