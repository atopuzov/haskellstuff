{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import           Control.Monad.Reader   (MonadReader, ReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Except   (ExceptT, MonadError)
import qualified Control.Exception      as E

import qualified Database.InfluxDB      as InfluxDB
import qualified Database.InfluxDB.Types  as InfluxDB.Types


data AppOptions = AppOptions {
  influxWp  :: InfluxDB.WriteParams
}
type AppConfig = MonadReader AppOptions

data AppError = IOError E.IOException

newtype App a = App {
  runApp :: ReaderT AppOptions (ExceptT AppError IO) a
  } deriving (Functor, Applicative, Monad, AppConfig, MonadIO, MonadError AppError)
