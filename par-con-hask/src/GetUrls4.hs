module GetUrls4 where
import GetURL

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (try, SomeException, throwIO)
import Control.Monad (when)

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO $ do
    r <- try action
    putMVar var r
  return (Async var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var


wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right a -> return a


-- Deadlocked version
data Async' a = Async' (MVar a)

async' :: IO a -> IO (Async' a)
async' action = do
  var <- newEmptyMVar
  forkIO $ do
    r <- action
    putMVar var r
  return (Async' var)

wait' :: Async' a -> IO a
wait' (Async' var) = readMVar var


ver1 = do
  a1 <- async (getURL "url1")
  a2 <- async (getURL "url2")

  r1 <- wait a1
  r2 <- wait a2
  print (length r1, length r2)

ver2 = do
  a1 <- async' (getURL "url1")
  a2 <- async' (getURL "url2")

  r1 <- wait' a1
  r2 <- wait' a2
  print (length r1, length r2)


main = do
  ver1
