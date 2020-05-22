module PhoneBook where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Prelude hiding (lookup)
import Control.Exception (bracket)

type Name = String
type PhoneNumber = String
type PhoneBook = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return $ PhoneBookState m

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  putMVar m (Map.insert name number book)


insert' :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert' (PhoneBookState m) name number = do
  book <- takeMVar m
  let book' = Map.insert name number book
  putMVar m book'
  seq book' $ return ()


lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return $ Map.lookup name book

lookup' (PhoneBookState m) name = do
  withMVar' m (\book -> return $ Map.lookup name book)


-- withMVar already present in Control.Concurrent.MVar
withMVar' :: MVar a -> (a -> IO b) -> IO b
withMVar' m f = bracket
  (takeMVar m)            -- acquire resource
  (\res -> putMVar m res) -- release resource
  (\res -> f res)         -- do work

main = do
  s <- new
  sequence_ [insert' s ("name" ++ show n) (show n) | n <- [1..10000]]
  lookup' s "name999" >>= print
  lookup' s "unknown" >>= print
