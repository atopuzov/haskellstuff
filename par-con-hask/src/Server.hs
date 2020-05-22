module Server where

import System.IO
import Control.Concurrent
import Control.Monad
import Network.Socket

talk :: Handle -> IO ()
talk h = do
  hSetNewlineMode h universalNewlineMode -- Swallow carriage returns sent by telnet clients
  hSetBuffering h LineBuffering
  loop
    where
      loop = do
        line <- hGetLine h
        if line == "end"
          then hPutStrLn h "Thank you for using the Haskell doubling service."
          else do
            hPutStrLn h (show (2 * (read line :: Integer)))
            loop

port :: Int
port = 44444

main = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just $ show port)
  -- bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
  bind sock $ addrAddress addr
  listen sock 1024
  putStrLn $ "Listening on port " ++ show port


  forever $ do
    (conn, peer) <- accept sock
    handle <- socketToHandle conn ReadWriteMode
    putStrLn $ "Accepted connection from: " ++ show peer
    forkFinally (talk handle) (\_ -> hClose handle)
