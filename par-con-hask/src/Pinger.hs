{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Pinger where

import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Distributed.Process (ProcessId, Process, expect, send, getSelfPid, say, terminate, spawn, getSelfNode)
import Control.Distributed.Process.Closure (remotable, mkStaticClosure)
import DistribUtils

data Message = Ping ProcessId
  | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message


pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ "ping received from " ++ (show from)
  mypid <- getSelfPid
  --
  send from (Pong mypid)

remotable ['pingServer]


master :: Process ()
master = do
  node <- getSelfNode
  say $ "spawning on " ++ (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ "my pid is " ++ (show mypid)
  say $ "sending ping to " ++ (show pid)
  send pid (Ping mypid) --
  Pong _ <- expect
  say "pong." --
  terminate

main :: IO ()
main = distribMain (\_ -> master) Pinger.__remoteTable
