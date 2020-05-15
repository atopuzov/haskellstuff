{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Types (
  CliOptions(..)
  , AppOptions(..)
  , AppError(..)
  , AppConfig(..)
  , Measurement(..)
  , App(..)
  ) where

import           Control.Monad.Except    (ExceptT, MonadError)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (MonadReader, ReaderT)

import           Control.Exception       (IOException)

import           Data.Aeson.Types        (FromJSON, withObject, (.:), (.=))
import           Data.Text               (Text)
import qualified Database.InfluxDB       as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types
import qualified Network.MQTT            as MQTT
import qualified Network.MQTT.Types      as MQTT.Types


data CliOptions = CliOptions {
    _mqttHost       :: !String
  , _mqttPort       :: !String
  , _mqttUsername   :: !Text
  , _mqttPassword   :: !Text
  , _mqttTopic      :: !Text
  , _influxHost     :: !Text
  , _influxDatabase :: !Text
  }

data AppOptions = AppOptions {
    mqttConfig :: !MQTT.Config
  , mqttTopic  :: !MQTT.Types.Topic
  , influxWp   :: !InfluxDB.WriteParams
  }

type AppConfig = MonadReader AppOptions

data AppError = IOError IOException

newtype App a = App {
  runApp :: ReaderT AppOptions (ExceptT AppError IO) a
  } deriving (Functor, Applicative, Monad, AppConfig, MonadIO, MonadError AppError)

data Measurement = SHT30 {
    mTemperature :: !Double
  , mHumidity    :: !Double
  , mClientID    :: !Text
} deriving Show

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" $ \o -> do
    mTemperature <- o .: "temperature"
    mHumidity    <- o .: "humidity"
    mClientID    <- o .: "client"
    return SHT30{..}
