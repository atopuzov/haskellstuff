module MVar1 where

import Control.Concurrent.MVar
import Control.Concurrent


main = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <-  takeMVar m
  print r
