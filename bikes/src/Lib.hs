{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( someFunc, Station(..)
    ) where

import Data.Aeson (decode, parseJSON, withObject, (.:), (.=))
import Data.Aeson.Types (FromJSON, Parser)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import qualified Data.Geohash
import qualified Data.Maybe

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
    standNumber :: Int
  , bikeStands :: Int
  , availableBikes :: Int
  , availableBikeStands :: Int
  , timestamp :: Int
  -- , lat :: Double
  -- , lng :: Double
  , geohash :: String
  } deriving Show

instance FromJSON Station where
  parseJSON = withObject "Station" $ \o -> do
    standNumber         <- o .: "number"
    bikeStands          <- o .: "bike_stands"
    availableBikes      <- o .: "available_bikes"
    availableBikeStands <- o .: "available_bike_stands"
    timestamp           <- o .: "last_update"
    position            <- o .: "position"
    lat                 <- position .: "lat" :: Parser Double
    lng                 <- position .: "lng" :: Parser Double
    let geohash = Data.Maybe.fromJust $ Data.Geohash.encode 10 (lat, lng)
    return Station{..}

getJson :: FromJSON a => IO a
getJson = do
  request <- parseRequest "https://api.jcdecaux.com"
  let request'
        = setRequestMethod "GET"
          $ setRequestPath "/vls/v1/stations"
          $ setRequestQueryString [ ("contract", Just "Dublin")
                                  , ("apiKey", Just "a360b2a061d254a3a5891e4415511251899f6df1")
                                  ]
          $ request
  response <- httpJSON request'
  return $ getResponseBody response

getBikes :: IO [Station]
getBikes = getJson

someFunc :: IO ()
someFunc = do
  manager <- newManager tlsManagerSettings

  bikes <- getBikes
  putStrLn $ show bikes
