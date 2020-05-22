module GetURL (getURL) where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (when)
import           Data.Monoid        ((<>))

getURL :: String -> IO String
getURL url = do
    putStrLn $ "Downloading " <> url
    threadDelay  (1 * 1000000)
    putStrLn $ "Downloading: 1 second has passed"
    -- when (url == "url1") $ error $ "connection aborted for" <> show url
    threadDelay  (1 * 1000000)
    putStrLn $ "Downloading: 2 seconds has passed"
    threadDelay  (1 * 1000000)
    putStrLn $ "Downloading: 3 seconds has passed"
    threadDelay  (1 * 1000000)
    putStrLn $ "Downloading " <> url <> ": Done"
    return $ "Contents of " <> url
