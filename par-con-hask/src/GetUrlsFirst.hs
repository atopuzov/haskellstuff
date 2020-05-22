module GetUrlsFirst where
import GetURL

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (try, SomeException, throwIO,  AsyncException(..))
import Control.Monad (when, forever)
import System.IO

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  tid <- forkFinally action (atomically . putTMVar var)
  return (Async tid var)

-- cancel :: Async a -> IO ()
-- cancel (Async tid _) = throwTo tid ThreadKilled

-- waitCatch :: Async a -> IO (Either SomeException a)
-- waitCatch (Async _ var) = readMVar var

-- wait :: Async a -> IO a
-- wait a = do
--   r <- waitCatch a
--   case r of
--     Left e -> throwIO e
--     Right a -> return a

-- -- waitEither :: Async a -> Async b -> IO (Either a b)
-- -- waitEither a b = do
-- --   m <- newEmptyMVar
-- --   forkIO $ do
-- --     r <- try (fmap Left $ wait a)
-- --     putMVar m r
-- --   forkIO $ do
-- --     r <- try (fmap Right $ wait b)
-- --     putMVar m r
-- --   wait (Async m)

-- -- waitAny :: [Async a] -> IO a
-- -- waitAny as = do
-- --   m <- newEmptyMVar
-- --   let forkwait  a = forkIO $ do
-- --         r <- try (wait a)
-- --         putMVar m r
-- --   mapM_ forkwait as
-- --   wait (Async m)

-- main= do
--   hSetBuffering stdin NoBuffering
--   let urls = ["url" <> show i | i <- [1..10]]
--   as <- mapM (async . getURL) urls

--   forkIO $ do
--     forever $ do
--       c <- getChar
--       when (c == 'q') $ mapM_ cancel as

--   print "Whoooo"
--   r <- mapM waitCatch as
--   print r

man = undefined
