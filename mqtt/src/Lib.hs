{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where
import           Types

import           Control.Concurrent      (forkIO)
import           Control.Monad           (forever, void)
import           Data.Aeson              (decodeStrict)
import qualified Network.MQTT            as MQTT

import           Control.Concurrent.STM  (newTChanIO, readTChan)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, ask, asks,
                                          runReaderT)
import           Control.Monad.STM       (atomically)
import qualified Data.Map                as Map
import           Data.Time.Clock         (UTCTime)
import qualified Database.InfluxDB       as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types
import qualified Network.MQTT.Types      as MQTT.Types
import           System.Exit             (exitFailure)
import           System.IO               (BufferMode (NoBuffering), hPutStrLn,
                                          hSetBuffering, stderr, stdout)


-- Write data to InfluxDB
writeData :: (AppConfig m, MonadIO m) => Measurement -> m ()
writeData val = do
  writeParams <- asks influxWp
  let client = Map.singleton "client" $ InfluxDB.Types.Key $ mClientID val

  liftIO $ InfluxDB.writeBatch writeParams
    [ InfluxDB.Line "temperature" client
      (Map.singleton "value" $ InfluxDB.FieldFloat $ mTemperature val)
      (Nothing :: Maybe UTCTime),
      InfluxDB.Line "humidity" client
      (Map.singleton "value"    $ InfluxDB.FieldFloat $ mHumidity val)
      (Nothing :: Maybe UTCTime)
    ]

-- Decode MQTT message
decodeMsg :: MQTT.Message 'MQTT.PUBLISH -> Maybe Measurement
decodeMsg = decodeStrict . MQTT.payload . MQTT.body

processMsgs config env = do
  msg <- fmap decodeMsg $ atomically $ readTChan (MQTT.cPublished config)
  case msg of
    Just value -> do
      putStrLn $ "Received: " ++ show value
      flip runReaderT env $ writeData value
    Nothing -> putStrLn "Unable to decode data."

app :: App ()
app = do
  config <- asks mqttConfig
  topic <- asks mqttTopic

  env <- ask

  _ <- liftIO . forkIO $ do
    qosGranted <- MQTT.subscribe config [(topic, MQTT.Handshake)]
    case qosGranted of
      [MQTT.Handshake] -> forever $ processMsgs config env
      _                -> do
        hPutStrLn stderr $ "Wanted QoS Handshake, got " ++ show qosGranted
        exitFailure

  -- this will throw IOExceptions
  -- Exception: <socket: 12>: hLookAhead: resource vanished (Connection reset by peer)
  -- Network.Socket.getAddrInfo (called with preferred socket type/protocol: AddrInfo {addrFlags = [AI_ADDRCONFIG], addrFamily = AF_UNSPEC, addrSocketType = Stream, addrProtocol = 6, addrAddress = <assumed to be undefined>, addrCanonName = <assumed to be undefined>}, host name: Just "dubpi.local", service name: Just "1883"): does not exist (System error)
  --  (ConnectionFailure Network.Socket.connect: <socket: 13>: does not exist (Connection refused)))
  terminated <- liftIO $ MQTT.run config
  liftIO $ hPutStrLn stderr $ "Application terminated: " ++ show terminated
