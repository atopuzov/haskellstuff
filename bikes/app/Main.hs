{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Lib

import           Control.Lens             ((&), (.~), (?~))
import           Control.Monad.Except    (runExceptT)
import           Control.Monad.Reader    (runReaderT)

import qualified Database.InfluxDB       as InfluxDB
import qualified Database.InfluxDB.Types as InfluxDB.Types

import           Control.Monad.IO.Class  (MonadIO, liftIO)


renderError :: AppError -> IO ()
renderError (IOError e) = do
    putStrLn "There was an error:"
    putStrLn $ "  " ++ show e

main :: IO ()
main = do
  let wp = InfluxDB.writeParams (InfluxDB.Types.Database $ "bikes") & InfluxDB.precision .~ InfluxDB.Second
  let appCfg = AppOptions wp "a360b2a061d254a3a5891e4415511251899f6df1" "Dublin"
  either renderError return =<< runExceptT (runReaderT (runApp app) appCfg)

app :: App ()
app = do
  bikes <- getBikes
  liftIO $ putStrLn $ "Read " ++ show (length bikes) ++ " station information"
  writeData bikes
  return ()
