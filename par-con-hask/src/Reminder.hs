module Reminder where

import Control.Concurrent
import Control.Monad


main = loop
  where
    loop = do
      t <- readLn
      if (t == 0)
        then return ()
        else do
          forkIO $ doReminder t
          loop

doReminder t = do
  threadDelay (10^6 * t)
  putStrLn "BEEP!"
