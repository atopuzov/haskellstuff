module GetUrls1 where
import GetURL

import Control.Concurrent
import Control.Concurrent.MVar

main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  r1 <- takeMVar m1
  r2 <- takeMVar m2
  print (length r1, length r2)
