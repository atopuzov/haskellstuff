module MVar3 where

import Control.Concurrent.MVar
import Control.Concurrent


main = do
  m <- newEmptyMVar
  takeMVar m
