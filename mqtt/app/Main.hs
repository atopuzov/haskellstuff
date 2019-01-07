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
import Control.Monad.Reader (ask, runReaderT, MonadReader, ReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (decodeStrict, parseJSON, withObject, (.:), (.=), object)
import Data.Aeson.Types (FromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Map as Map

import qualified Database.InfluxDB as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types
import qualified Network.MQTT as MQTT

data AppConfig = AppConfig {
  mqttConfig :: MQTT.Config
  , influxWp :: InfluxDB.WriteParams
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
writeData :: (MonadReader AppConfig m, MonadIO m) => Measurement -> m ()
writeData val = do
  writeParams <- influxWp <$> ask

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
decodeMsg mqttMessage = decodeStrict payload :: Maybe Measurement
  where
    payload = MQTT.payload . MQTT.body $ mqttMessage

main :: IO ()
main = do
  cmds <- MQTT.mkCommands
  chan <- newTChanIO

  let config = (MQTT.defaultConfig cmds chan) {   MQTT.cHost = "10.147.19.189"
                                                , MQTT.cClientID = "haskell-mqtt-2"
                                                , MQTT.cKeepAlive = Just 10
                                              }

  let wp = InfluxDB.writeParams (InfluxDB.Types.Database $ "temperature") & InfluxDB.precision .~ InfluxDB.Second

  let appCfg = AppConfig config wp
  runReaderT (runApp app) appCfg

-- app :: App ()
app = do
  config <- mqttConfig <$> ask

  liftIO $ do
    mqtt <- async $ void $ MQTT.run config
    MQTT.subscribe config [("outTopic", MQTT.Handshake)]

  forever $ do
    message <- liftIO $ atomically $ readTChan (MQTT.cPublished config)

    let decoded = decodeMsg message
    case decoded of
      Just value -> do
        liftIO $ putStrLn $ "Received: " ++ show value
        writeData value
      Nothing -> liftIO $ putStrLn "Unable to decode data."
