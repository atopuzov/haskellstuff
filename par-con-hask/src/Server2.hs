module Server2 where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Network.Socket
import System.IO

talk :: Handle -> TVar Integer -> IO ()
talk h f = do
  hSetNewlineMode h universalNewlineMode -- Swallow carriage returns sent by telnet clients
  hSetBuffering h LineBuffering
  c <- atomically newTChan
  race (srv h f c) (rcv h c)
  return ()

srv :: Handle -> TVar Integer -> TChan String -> IO ()
srv h factor c = do
  f <- atomically $ readTVar factor
  hPutStrLn h $ "Current factor:" ++ show f
  loop f
  where
    loop f = do
      action <- atomically $ do
        f' <- readTVar factor
        if (f /= f')
          then return (newfactor f')
          else do
            l <- readTChan c
            return (command f l)
      action
    newfactor f = do
      hPutStrLn h $ "new factor: " ++ show f
      loop f
    command f s = case s of
      "end" -> hPutStrLn h "Thank you for using the Haskell doubling service."
      '*' : s -> do
        atomically $ writeTVar factor (read s :: Integer)
        loop f
      line -> do
        hPutStrLn h (show (f * (read line :: Integer)))
        loop f

rcv h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line

port :: Int
port = 44444

main = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  addr : _ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just $ show port)
  -- bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
  bind sock $ addrAddress addr
  listen sock 1024
  putStrLn $ "Listening on port " ++ show port
  factor <- atomically $ newTVar 2
  forever $ do
    (conn, peer) <- accept sock
    handle <- socketToHandle conn ReadWriteMode
    putStrLn $ "Accepted connection from: " ++ show peer
    forkFinally (talk handle factor) (\_ -> hClose handle)
