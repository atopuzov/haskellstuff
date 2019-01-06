{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE NoImplicitPrelude          #-}
module Main where

import Lib

import Control.Concurrent.Async (async)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan)
import Control.Lens ((?~), (.~), (&))
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Monad.Reader (ask, runReaderT, MonadReader, ReaderT, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (decodeStrict, parseJSON, toJSON, withObject, (.:), (.=), object)
import Data.Aeson.Types (FromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map

import qualified Database.InfluxDB as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types
import qualified Network.MQTT as MQTT

data AppConfig = AppConfig {
  mqttHost :: Text
  , mqttTopic :: Text
  , influxDbDatabase :: Text
  }

newtype App a = App {
  runApp :: ReaderT AppConfig IO a
  } deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO)

data Measurement = SHT30 {
    mTemperature :: Double
  , mHumidity :: Double
  , mClientID :: Text
} deriving Show

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" $ \o -> do
    mTemperature <- o .: "temperature"
    mHumidity    <- o .: "humidity"
    mClientID    <- o .: "client"
    return SHT30{..}

-- Write data to InfluxDB
writeData wp val = do
  let client = Map.singleton "client" $ InfluxDB.Types.Key $ mClientID val

  InfluxDB.writeBatch wp
    [ InfluxDB.Line "temperature" client
      (Map.singleton "value" $ InfluxDB.FieldFloat $ mTemperature val)
      (Nothing :: Maybe UTCTime),
      InfluxDB.Line "humidity" client
      (Map.singleton "value"    $ InfluxDB.FieldFloat $ mHumidity val)
      (Nothing :: Maybe UTCTime)
    ]

-- Decode MQTT message
handleMsg msg = do
  let payload = MQTT.payload . MQTT.body $ msg
  return (decodeStrict payload :: Maybe Measurement)

main :: IO ()
main = do
  let appCfg = AppConfig "dubpi.local" "outTopic" "temperature"
  runReaderT (runApp app) appCfg

app :: App ()
app = do
  cmds <- liftIO MQTT.mkCommands
  chan <- liftIO newTChanIO

  env <- ask
  let config = (MQTT.defaultConfig cmds chan) {   MQTT.cHost = "10.147.19.189"
                                                , MQTT.cClientID = "haskell-mqtt"
                                                , MQTT.cKeepAlive = Just 10
                                              }
  let wp = InfluxDB.writeParams (InfluxDB.Types.Database $ influxDbDatabase env) & InfluxDB.precision .~ InfluxDB.Second


  liftIO $ do
    mqtt <- async $ void $ MQTT.run config
    MQTT.subscribe config [("outTopic", MQTT.Handshake)]
    forever $ do
      message <- atomically $ readTChan (MQTT.cPublished config)
      decoded <- handleMsg message
      case decoded of
        Just value -> do
          putStrLn $ "Received: " ++ show value
          writeData wp value
        Nothing -> putStrLn "Unable to decode data."
