{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import           Control.Exception      (IOException)
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import qualified Database.InfluxDB      as InfluxDB

data AppOptions = AppOptions {
    influxWp :: InfluxDB.WriteParams
  , apiKey   :: String
}

type AppConfig = MonadReader AppOptions

data AppError = IOError IOException

type AppState a = ReaderT AppOptions (ExceptT AppError IO) a

newtype App a = App {
  runApp :: AppState a
  } deriving ( Functor
             , Applicative
             , Monad
             , AppConfig
             , MonadIO
             , MonadError AppError
             )
