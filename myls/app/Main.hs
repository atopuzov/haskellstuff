{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Foldable as F
import Data.Traversable as T
import System.Directory as D
import Options.Applicative as O
import System.Console.ANSI as A
import Control.Monad.Reader as R
import Data.List (sortBy)
import Data.Ord as OR

data FileInfo = FileInfo {
  _filename :: String,
  _size :: Integer,
  _isdir :: Bool
  } deriving Show

data MyLsOptions = MyLsOptions {
  _path :: String,
  _long :: Bool,
  _sortMode :: SortMode
  }

data SortMode = Filename | Size | FilenameLength deriving Read

type MyApp = R.ReaderT MyLsOptions IO

main :: IO ()
main = do
  opts <- O.execParser cliParserInfo
  R.runReaderT body opts

body :: MyApp ()
body =  do
  getFiles >>= elaborateFiles >>= sortFiles >>= printFiles

sortFiles :: [FileInfo] -> MyApp [FileInfo]
sortFiles files = do
  mode <- _sortMode <$> ask
  let comparator = case mode of
        Size -> OR.comparing _size
        Filename -> OR.comparing _filename
        FilenameLength -> OR.comparing $ length . _filename
  pure (sortBy comparator files)

getFiles ::MyApp [String]
getFiles = do
  opts <- ask
  liftIO $ D.setCurrentDirectory (_path opts)
  liftIO $ D.listDirectory "."

printFiles :: [FileInfo] -> MyApp ()
printFiles files = do
  opts <- ask
  liftIO $ if _long opts
           then printLongFiles files
           else printShortFiles files

cliParser :: O.Parser MyLsOptions
cliParser =
  MyLsOptions <$> 
  O.strArgument (O.metavar "DIRECTORY" <> O.value ".")
  <*> O.switch (O.long "long" <> O.short 'l' <> O.help "enable long output")
  <*> O.option O.auto (O.long "sort" <> O.value Filename)

cliParserInfo :: O.ParserInfo MyLsOptions
cliParserInfo = O.info (O.helper <*> cliParser) (mempty)

elaborateFiles :: [String] -> MyApp [FileInfo]
elaborateFiles files =
  T.for files $ \file ->
                 FileInfo <$> (pure file)
                 <*> (liftIO $ D.getFileSize file)
                 <*> (liftIO $ D.doesDirectoryExist file)

printLongFiles :: [FileInfo] -> IO ()
printLongFiles files = F.for_ files putFileInfoLn

printShortFiles :: [FileInfo] -> IO ()
printShortFiles files = F.for_ files (putStrLn . _filename)


setColor :: A.Color -> IO ()
setColor color = A.setSGR [A.SetColor A.Foreground A.Vivid color]

setNormal :: IO ()
setNormal = A.setSGR []

putFileInfoLn :: FileInfo -> IO ()
putFileInfoLn file = do
  putStr (_filename file)
  putStr "."
  if _isdir file
    then do
    setColor A.Green
    putStr "directory"
    setNormal
    else do
    setColor A.Cyan
    (putStr . prettySize . _size) file
    setNormal
  putStrLn ""
  
  -- putStrLn (_filename file ++ ", " ++ prettySize (_size file) ++ " " ++ show (_isdir file))



prettySize :: Integer -> String
prettySize (prefixify -> (s,p)) | s == 1 = show s ++ " " ++ p ++ "byte"
prettySize (prefixify -> (s,p)) = show s ++ " " ++ p ++ "bytes"

-- prettySize 1 = "1 bype"
-- prettySize s | s < 1024 = show (s `div` 1024) ++ " kilobytes"
-- prettySize 1024 = "1 kilobyte"
-- prettySize s = show s ++ " bytes"


prefixify :: Integer -> (Integer, String)
prefixify n | n >= 1024*1024 = ((n `div` (1024*1024)), "mega")
prefixify n | n >= 1024 = ((n `div` 1024), "kilo")
prefixify n = (n, "")
