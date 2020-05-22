module FindPar where

import Control.Concurrent.Async
import System.Directory
import System.FilePath
import Data.List (sort)


subfind :: String ->
           FilePath ->
           ([Async (Maybe FilePath)] -> IO (Maybe FilePath)) ->
           [Async (Maybe FilePath)] ->
           IO (Maybe FilePath)
subfind s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
    then inner asyncs
    else withAsync (find s p) $ \a -> inner (a:asyncs)

find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else do
    let ps = map (d </>) fs'
    foldr (subfind s) dowait ps []
 where
   dowait as = loop (reverse as)
   loop [] = return Nothing
   loop (a:as) = do
     r <- wait a
     case r of
       Nothing -> loop as
       Just a -> return (Just a)
