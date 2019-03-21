{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Lib
import           Config

import           Control.Lens             ((&), (.~), (?~))
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.Reader   (runReaderT)

import qualified Database.InfluxDB      as InfluxDB
import qualified Database.InfluxDB.Types  as InfluxDB.Types



main :: IO ()
main = do
  let wp = InfluxDB.writeParams (InfluxDB.Types.Database $ "bikes") & InfluxDB.precision .~ InfluxDB.Second
  someFunc
