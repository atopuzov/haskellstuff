{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types where

import           Data.Aeson            (parseJSON, withObject, (.:), (.=))
import           Data.Aeson.Types      (FromJSON, Parser)
import qualified Data.Geohash
import qualified Data.Maybe
import           Data.Time             (UTCTime (..))
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- {"number":42,
--  "contract_name":"Dublin",
--  "name":"SMITHFIELD NORTH",
--  "address":"Smithfield North",
--  "position":{"lat":53.349562,"lng":-6.278198},
--  "banking":true,
--  "bonus":false,
--  "bike_stands":30,
--  "available_bike_stands":1,
--  "available_bikes":29,
--  "status":"OPEN",
--  "last_update":1552330831000},

data Station = Station {
    standNumber         :: Int
  , bikeStands          :: Int
  , availableBikes      :: Int
  , availableBikeStands :: Int
  , timestamp           :: UTCTime
  , geohash             :: String
  } deriving Show

instance FromJSON Station where
  parseJSON = withObject "Station" $ \o -> do
    standNumber         <- o .: "number"
    bikeStands          <- o .: "bike_stands"
    availableBikes      <- o .: "available_bikes"
    availableBikeStands <- o .: "available_bike_stands"
    timestamp           <- fmap (posixSecondsToUTCTime . (/1000)) (o .: "last_update")
    position            <- o .: "position"
    lat                 <- position .: "lat" :: Parser Double
    lng                 <- position .: "lng" :: Parser Double
    let geohash = Data.Maybe.fromJust $ Data.Geohash.encode 10 (lat, lng)
    return Station{..}
