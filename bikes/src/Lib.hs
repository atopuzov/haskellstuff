{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Config
import           Types

import qualified Control.Exception       as E
import           Control.Monad.Except    (MonadError, MonadIO, throwError)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Aeson              (decode, parseJSON, withObject, (.:),
                                          (.=))
import           Data.Aeson.Types        (FromJSON, Parser)
import qualified Data.ByteString.Char8   as C


import qualified Data.Map                as Map
import           Data.Text               (pack)
import qualified Database.InfluxDB       as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

import           Control.Lens            ((&), (.~), (?~))
import           Control.Monad.Reader    (runReaderT)


-- https://developer.jcdecaux.com/#/home
-- https://developer.jcdecaux.com/#/opendata/vls?page=getstarted
-- https://developer.jcdecaux.com/#/opendata/vls?page=static
-- https://developer.jcdecaux.com/#/opendata/vls?page=dynamic

getBikes :: (MonadIO m, AppConfig m) => m [Station]
getBikes = do
  request <- liftIO $ parseRequest "https://api.jcdecaux.com"
  apiK <- C.pack <$> asks apiKey
  con <- C.pack <$> asks contract

  let request'
        = setRequestMethod "GET"
          $ setRequestPath "/vls/v1/stations"
          $ setRequestQueryString
          [ ("contract", Just con)
          , ("apiKey", Just apiK)
          ]
          $ request

  response <- liftIO $ httpJSON request'
  return $ getResponseBody response

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
       (Just $ timestamp val)
     ) vals
