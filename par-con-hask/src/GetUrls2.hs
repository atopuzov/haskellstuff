module GetUrls2 where
import GetURL

import Control.Concurrent
import Control.Concurrent.MVar

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO $ do
    r <- action
    putMVar var r
  return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var


main = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")

  r1 <- wait a1
  r2 <- wait a2
  print (length r1, length r2)
