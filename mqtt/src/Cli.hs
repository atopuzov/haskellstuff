{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cli (
  cliParserInfo
  ) where

import qualified Options.Applicative as OA
import           Types               (CliOptions (..))


cliParser :: OA.Parser CliOptions
cliParser = do
  _mqttHost <- OA.option OA.str $
    OA.long "mqtt-host" <>
    OA.value "localhost" <>
    OA.help "MQTT host to connect to"

  _mqttPort <- OA.option OA.str $
    OA.long "mqtt-port" <>
    OA.value "1883" <>
    OA.help "MQTT port to connect to"

  _mqttUsername <- OA.option OA.str $
    OA.long "mqtt-username" <>
    OA.short 'u' <>
    OA.help "MQTT username"

  _mqttPassword <- OA.option OA.str $
    OA.long "mqtt-password" <>
    OA.short 'p' <>
    OA.help "MQTT password"

  _mqttTopic <- OA.option OA.str $
    OA.long "mqtt-topic" <>
    OA.value "outTopic" <>
    OA.help "MQTT topic"

  _influxHost <- OA.option OA.str $
    OA.long "influx-host" <>
    OA.value "localhost" <>
    OA.help "InfluxDB host to connect to"

  _influxDatabase <- OA.option OA.str $
    OA.long "influx-db" <>
    OA.value "temperature" <> OA.help "InfluxDB database"

  pure CliOptions {..}

cliParserInfo :: OA.ParserInfo CliOptions
cliParserInfo = OA.info (OA.helper <*> cliParser) mempty
