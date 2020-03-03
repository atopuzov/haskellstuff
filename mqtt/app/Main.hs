{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE NoImplicitPrelude          #-}
module Main where

import           Cli
import           Lib
import           Types

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
import           Data.Aeson               (decodeStrict)
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

import qualified Network.Connection       as NC

renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e


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
        , MQTT.cTLS       = Just NC.TLSSettingsSimple
                            { settingDisableCertificateValidation = False
                            , settingDisableSession = False
                            , settingUseServerName = False
                            }
        , MQTT.cClientID  = UUID.toText $ fromJust clientId
        , MQTT.cKeepAlive = Just 10
        , MQTT.cUsername  = Just $ _mqttUsername opts
        , MQTT.cPassword  = Just $ _mqttPassword opts
        }
  let influxWp = InfluxDB.writeParams (InfluxDB.Types.Database $ _influxDatabase opts) & InfluxDB.precision .~ InfluxDB.Second
  let topic = MQTT.Types.toTopic $ MQTT.Types.MqttText $ _mqttTopic opts
  let appCfg = AppOptions mqttConfig topic influxWp

  either renderError return =<< runExceptT (runReaderT (runApp app) appCfg)
