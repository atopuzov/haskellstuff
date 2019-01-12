{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE NoImplicitPrelude          #-}
module Main where

import           Lib

import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (newTChanIO, readTChan)
import           Control.Lens             ((&), (.~), (?~))
import           Control.Monad            (forever, void)
import           Control.Monad.Except     (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask, asks,
                                           runReaderT)
import           Control.Monad.STM        (atomically)
import           Data.Aeson               (decodeStrict, parseJSON, withObject,
                                           (.:), (.=))
import           Data.Aeson.Types         (FromJSON)
import           Data.Maybe               (fromJust)
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           System.Exit              (exitFailure)
import           System.IO                (BufferMode (NoBuffering), hPutStrLn,
                                           hSetBuffering, stderr, stdout)

import qualified Control.Exception        as E
import qualified Data.Map                 as Map
import qualified Data.UUID                as UUID
import qualified Data.UUID.V1             as UUID.V1
import qualified Database.InfluxDB        as InfluxDB
import qualified Database.InfluxDB.Types  as InfluxDB.Types
import qualified Network.MQTT             as MQTT
import qualified Network.MQTT.Types       as MQTT.Types

data AppOptions = AppOptions {
  mqttConfig  :: MQTT.Config
  , mqttTopic :: MQTT.Types.Topic
  , influxWp  :: InfluxDB.WriteParams
  }

type AppConfig = MonadReader AppOptions

data AppError = IOError E.IOException

newtype App a = App {
  runApp :: ReaderT AppOptions (ExceptT AppError IO) a
  } deriving (Functor, Applicative, Monad, AppConfig, MonadIO, MonadError AppError)

data Measurement = SHT30 {
    mTemperature :: Double
  , mHumidity    :: Double
  , mClientID    :: Text
} deriving Show

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" $ \o -> do
    mTemperature <- o .: "temperature"
    mHumidity    <- o .: "humidity"
    mClientID    <- o .: "client"
    return SHT30{..}

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

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cmds <- MQTT.mkCommands
  chan <- newTChanIO

  clientId <- UUID.V1.nextUUID
  let config = (MQTT.defaultConfig cmds chan) {
                                                  MQTT.cHost = "dubpi.local"
                                                , MQTT.cClientID = UUID.toText $ fromJust clientId
                                                , MQTT.cKeepAlive = Just 10
                                                -- , MQTT.cLogDebug = putStrLn
                                              }

  let wp = InfluxDB.writeParams (InfluxDB.Types.Database $ "temperature") & InfluxDB.precision .~ InfluxDB.Second
  let topic = MQTT.Types.toTopic $ MQTT.Types.MqttText "outTopic"
  let appCfg = AppOptions config topic wp

  either renderError return =<< runExceptT (runReaderT (runApp app) appCfg)

app :: App ()
app = do
  config <- asks mqttConfig
  topic <- asks mqttTopic

  env <- ask

  _ <- liftIO . forkIO $ do
    qosGranted <- MQTT.subscribe config [(topic, MQTT.Handshake)]
    case qosGranted of
      [MQTT.Handshake] -> forever $ do
        msg <- fmap decodeMsg $ atomically $ readTChan (MQTT.cPublished config)
        case msg of
          Just value -> do
            hPutStrLn stdout $ "Received: " ++ show value
            runReaderT (writeData value) env
          Nothing -> putStrLn "Unable to decode data."
      _ -> do
        hPutStrLn stderr $ "Wanted QoS Handshake, got " ++ show qosGranted
        exitFailure

  -- this will throw IOExceptions
  -- Exception: <socket: 12>: hLookAhead: resource vanished (Connection reset by peer)
  -- Network.Socket.getAddrInfo (called with preferred socket type/protocol: AddrInfo {addrFlags = [AI_ADDRCONFIG], addrFamily = AF_UNSPEC, addrSocketType = Stream, addrProtocol = 6, addrAddress = <assumed to be undefined>, addrCanonName = <assumed to be undefined>}, host name: Just "dubpi.local", service name: Just "1883"): does not exist (System error)
  --  (ConnectionFailure Network.Socket.connect: <socket: 13>: does not exist (Connection refused)))
  terminated <- liftIO $ MQTT.run config
  liftIO $ hPutStrLn stderr $ "Terminated:" ++ show terminated
