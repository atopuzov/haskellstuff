module GetUrls6 where
import GetURL

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (try, SomeException, throwIO)
import Control.Monad (when)
import Data.Monoid ((<>))

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

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = do
  m <- newEmptyMVar
  forkIO $ do
    r <- try (fmap Left $ wait a)
    putMVar m r
  forkIO $ do
    r <- try (fmap Right $ wait b)
    putMVar m r
  wait (Async m)

waitAny :: [Async a] -> IO a
waitAny as = do
  m <- newEmptyMVar
  let forkwait  a = forkIO $ do
        r <- try (wait a)
        putMVar m r
  mapM_ forkwait as
  wait (Async m)

main = do
  -- a1 <- async (getURL "url1")
  -- a2 <- async (getURL "url2")

  -- r <- waitEither a1 a2
  -- print r
  let urls = ["url" <> show i | i <- [1..10]]
  as <- mapM (async . getURL) urls
  r <- waitAny as
  print r
  mapM_ wait as
