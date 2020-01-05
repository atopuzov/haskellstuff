{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE NoImplicitPrelude          #-}
module Main where

import           Lib

import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (newTChanIO, readTChan)
import qualified Control.Exception        as E
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
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust)
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import qualified Data.UUID                as UUID
import qualified Data.UUID.V1             as UUID.V1
import qualified Database.InfluxDB        as InfluxDB
import qualified Database.InfluxDB.Types  as InfluxDB.Types
import qualified Network.MQTT             as MQTT
import qualified Network.MQTT.Types       as MQTT.Types
import qualified Options.Applicative      as OA
import           System.Exit              (exitFailure)
import           System.IO                (BufferMode (NoBuffering), hPutStrLn,
                                           hSetBuffering, stderr, stdout)

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
    mqttConfig  :: !MQTT.Config
  , mqttTopic   :: !MQTT.Types.Topic
  , influxWp    :: !InfluxDB.WriteParams
  }

type AppConfig = MonadReader AppOptions

data AppError = IOError E.IOException

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

cliParser :: OA.Parser CliOptions
cliParser = do
  _mqttHost <- OA.option OA.str
    (OA.long "mqtt-host" <> OA.value "localhost" <> OA.help "MQTT host to connect to")
  _mqttPort <- OA.option OA.str
    (OA.long "mqtt-port" <> OA.value "1883" <> OA.help "MQTT port to connect to")
  _mqttUsername <- OA.option OA.str
    (OA.long "mqtt-username" <> OA.short 'u' <> OA.help "MQTT username")
  _mqttPassword <- OA.option OA.str
    (OA.long "mqtt-password" <> OA.short 'p' <> OA.help "MQTT password")
  _mqttTopic <- OA.option OA.str
    (OA.long "mqtt-topic" <> OA.value "outTopic" <> OA.help "MQTT topic")
  _influxHost <- OA.option OA.str
    (OA.long "influx-host" <> OA.value "localhost" <> OA.help "InfluxDB host to connect to")
  _influxDatabase <- OA.option OA.str
    (OA.long "influx-db" <> OA.value "temperature" <> OA.help "InfluxDB database")
  pure CliOptions {..}

cliParserInfo :: OA.ParserInfo CliOptions
cliParserInfo = OA.info (OA.helper <*> cliParser) (mempty)

main :: IO ()
main = do
  opts <- OA.execParser cliParserInfo

  hSetBuffering stdout NoBuffering
  cmds <- MQTT.mkCommands
  chan <- newTChanIO

  clientId <- UUID.V1.nextUUID
  let mqttConfig = (MQTT.defaultConfig cmds chan) {
          MQTT.cHost      = _mqttHost opts
        , MQTT.cPort      = read $ _mqttPort opts
        , MQTT.cClientID  = UUID.toText $ fromJust clientId
        , MQTT.cKeepAlive = Just 10
        , MQTT.cUsername  = Just $ _mqttUsername opts
        , MQTT.cPassword  = Just $ _mqttPassword opts
        }
  let influxWp = InfluxDB.writeParams (InfluxDB.Types.Database $ _influxDatabase opts) & InfluxDB.precision .~ InfluxDB.Second
  let topic = MQTT.Types.toTopic $ MQTT.Types.MqttText $ _mqttTopic opts
  let appCfg = AppOptions mqttConfig topic influxWp

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
            putStrLn $ "Received: " ++ show value
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
  liftIO $ hPutStrLn stderr $ "Application terminated: " ++ show terminated
