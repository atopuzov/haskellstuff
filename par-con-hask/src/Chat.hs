{-# LANGUAGE RecordWildCards #-}
module Chat where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Network.Socket
import System.IO
import Text.Printf

type ClientName = String

data Client
  = Client
      { clientName :: ClientName,
        clientHandle :: Handle,
        clientKicked :: TVar (Maybe String),
        clientSendChan :: TChan Message
      }

data Message
  = Notice String
  | Tell ClientName String
  | Brdcast ClientName String
  | Command String

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
  c <- newTChan
  k <- newTVar Nothing
  return
    Client
      { clientName = name,
        clientHandle = handle,
        clientSendChan = c,
        clientKicked = k
      }

sendMessage :: Client -> Message -> STM ()
sendMessage Client {..} msg =
  writeTChan clientSendChan msg

data Server = Server {clients :: TVar (Map ClientName Client)}

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  return Server {clients = c}

broadcast :: Server -> Message -> STM ()
broadcast Server {..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)

port :: Int
port = 44444

main = withSocketsDo $ do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  addr : _ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just $ show port)
  bind sock $ addrAddress addr
  listen sock 1024
  putStrLn $ "Listening on port " ++ show port
  server <- newServer
  forever $ do
    (conn, peer) <- accept sock
    handle <- socketToHandle conn ReadWriteMode
    putStrLn $ "Accepted connection from: " ++ show peer
    forkFinally (talk handle server) (\_ -> hClose handle)

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server {..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do
      client <- newClient name handle
      writeTVar clients $ Map.insert name client clientmap
      broadcast server $ Notice (name ++ " has connected")
      return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server {..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server@Server {..} = do
  hSetNewlineMode handle universalNewlineMode -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if Prelude.null name
        then readName
        else mask $ \restore -> do
          ok <- checkAddClient server name handle --
          case ok of
            Nothing -> restore $ do
              hPrintf handle "The name %s is in use, please choose another\n" name
              readName
            Just client -> restore (runClient server client) `finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient serv@Server {..} client@Client {..} = do
  race server receive
  return ()
  where
    receive = forever $ do
      msg <- hGetLine clientHandle
      atomically $ sendMessage client (Command msg)
    server = join $ atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason ->
          return
            $ hPutStrLn clientHandle
            $ "You have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage serv client msg
            when continue $ server

tell :: Server -> Client -> ClientName -> String -> STM ()
tell serv@Server {..} client@Client {..} who msg = do
  clientmap <- readTVar clients
  let lkp = Map.lookup who clientmap
  case lkp of
    Nothing -> return ()
    Just dest -> sendMessage dest (Tell clientName msg)

kick :: Server -> ClientName -> ClientName -> STM ()
kick serv@Server {..} who cl = do
  clientmap <- readTVar clients
  let lkp = Map.lookup who clientmap
  case lkp of
    Nothing -> return ()
    Just client@Client {..} ->
      writeTVar clientKicked (Just cl)

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client {..} message =
  case message of
    Notice msg -> output $ "*** " ++ msg
    Tell name msg -> output $ "*" ++ name ++ "*: " ++ msg
    Brdcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
    Command msg -> case words msg of
      ["/kick", who] -> do
        atomically $ kick server who clientName
        return True
      "/tell" : who : what -> do
        atomically $ tell server client who (unwords what)
        return True
      ["/quit"] ->
        return False
      ('/' : _) : _ -> do
        hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
        return True
      _ -> do
        atomically $ broadcast server $ Brdcast clientName msg
        return True
  where
    output s = do hPutStrLn clientHandle s; return True
