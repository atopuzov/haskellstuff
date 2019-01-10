{-# LANGUAGE FlexibleContexts #-}
-- FelixbleContexts required for MonadReader, MonadIO
-- Based on https://hackernoon.com/the-reader-monad-part-1-1e4d947983a8

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Need 2 files in the current directory
-- test.txt (read by the load function)
-- history5.txt (read by the loadRevsion function)

newtype Directory = Directory String
newtype Filename = Filename String

instance Show Directory where
  show (Directory x) = x
instance Show Filename where
  show (Filename x) = x

type MyOut = (String, String)

myDirectory :: Directory
myDirectory = Directory "./"

myFile :: Filename
myFile = Filename "test.txt"

-- treading the parametar
load :: Directory -> Filename -> IO String
load directory filename = readFile (show directory ++ show filename)

loadRevision :: Directory -> Int -> IO String
loadRevision directory version = load directory $ Filename ("history" ++ show version ++ ".txt")

loadAll :: Directory -> Int -> Filename -> IO MyOut
loadAll directory version filename = do
    a <- load directory filename
    b <- loadRevision directory version
    return (a, b)

runLoad = loadAll myDirectory 5 myFile

-- Using ReaderT
type RTDirIO = ReaderT Directory IO

loadRT :: Filename -> RTDirIO String
loadRT filename = do
  directory <- ask
  liftIO $ readFile (show directory ++ show filename)

loadRevisionRT :: Int -> RTDirIO String
loadRevisionRT version = loadRT $ Filename ("history" ++ show version ++ ".txt")

loadAllRT :: Int -> Filename -> RTDirIO MyOut
loadAllRT version filename = do
    a <- loadRT filename
    b <- loadRevisionRT version
    return (a, b)

runLoadRT :: IO MyOut
runLoadRT = (runReaderT (loadAllRT 5 myFile)) myDirectory

-- Using MonadReader ad MonadIO
-- requires FlexibleContexts
loadMRI :: (MonadReader Directory m, MonadIO m) => Filename -> m String
loadMRI filename = do
  directory <- ask
  liftIO $ readFile (show directory ++ show filename)

loadRevisionMRI :: (MonadReader Directory m, MonadIO m) => Int -> m String
loadRevisionMRI version = loadMRI $ Filename ("history" ++ show version ++ ".txt")

loadAllMRI :: (MonadReader Directory m, MonadIO m) => Int -> Filename -> m MyOut
loadAllMRI version filename = do
    a <- loadMRI filename
    b <- loadRevisionMRI version
    return (a, b)

runLoadMRI :: (MonadIO m) => m MyOut
runLoadMRI = (runReaderT (loadAllMRI 5 myFile)) myDirectory

-- -- Test it out
main :: IO ()
main = do
  putStrLn "threading trough the functions"
  (file, history) <- runLoad
  putStr file
  putStr history

  putStrLn "using ReaderT (transformer) monad"
  (fileRT, historyRT) <- runLoadRT
  putStr fileRT
  putStr historyRT

  putStrLn "using MonadReader and MonadIO"
  (fileMRI, historyMRI) <- runLoadMRI
  putStr fileMRI
  putStr historyMRI
