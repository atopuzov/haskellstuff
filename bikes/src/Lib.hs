{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Config
import           Types

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Aeson              (decode, parseJSON, withObject, (.:),
                                          (.=))
import           Data.Aeson.Types        (FromJSON, Parser)
import qualified Data.Map                as Map
import           Data.Text               (pack)
import           Data.Time.Clock         (UTCTime)
import qualified Database.InfluxDB       as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple


getJson :: (MonadIO m, FromJSON a) => m a
getJson = do
  request <- liftIO $ parseRequest "https://api.jcdecaux.com"
  let request'
        = setRequestMethod "GET"
          $ setRequestPath "/vls/v1/stations"
          $ setRequestQueryString [ ("contract", Just "Dublin")
                                  , ("apiKey", Just "a360b2a061d254a3a5891e4415511251899f6df1")
                                  ]
          $ request
  response <- liftIO $ httpJSON request'
  return $ getResponseBody response

getBikes :: IO [Station]
getBikes = getJson

someFunc :: IO ()
someFunc = do
  manager <- newManager tlsManagerSettings

  bikes <- getBikes
  putStrLn $ show bikes


writeData :: (AppConfig m, MonadIO m) => [Station] -> m ()
writeData vals = do
  writeParams <- asks influxWp

  -- tags:   id (standNumber), geohash (geohash)
  -- values: bikeStands, availableBikes, availableBikeStands
  liftIO $ InfluxDB.writeBatch writeParams $ fmap
     (\val -> InfluxDB.Line "bikes"
       (Map.fromList
        [ ("id",      InfluxDB.Types.Key . pack . show . standNumber $ val),
          ("geohash", InfluxDB.Types.Key . pack . geohash $ val)
        ])
       (Map.fromList
        [ ("stands",    InfluxDB.FieldInt . fromIntegral . bikeStands $ val)
        , ("bikes",     InfluxDB.FieldInt . fromIntegral . availableBikes $ val)
        , ("available", InfluxDB.FieldInt . fromIntegral . availableBikeStands $ val)
        ])
       (Nothing :: Maybe UTCTime) -- timestamp is present in station
     ) vals
