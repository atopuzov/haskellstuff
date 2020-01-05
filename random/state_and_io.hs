{-# LANGUAGE FlexibleContexts #-}
-- FelixbleContexts required for MonadState, MonadIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, StateT, get, put, runStateT, evalStateT, execStateT)


tick :: StateT Int IO ()
tick = do
  x <- get
  liftIO $ putStrLn ("incrementing " ++ (show x))
  put $ x + 1

tickTick = do
  tick
  tick

runTickTick = runStateT tickTick 10

--
tick' :: (MonadIO m, MonadState Int m) => m ()
tick' = do
  x <- get
  liftIO $ putStrLn ("incrementing " ++ (show x))
  put $ x + 1

tickTick' = do
  tick'
  tick'

runTickTick' = runStateT tickTick' 10

main :: IO ()
main = do
  runTickTick
  runTickTick'
  putStrLn ""
