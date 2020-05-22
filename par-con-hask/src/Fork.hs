module Fork where

import Control.Concurrent
import Control.Monad
import System.IO


message n l = replicateM_ n $ putStr l

main = do
  hSetBuffering stdout NoBuffering
  let times = 50
  tid1 <- forkIO $ message times "A"
  tid2 <- forkIO $ message times "B"
  print tid1
  print tid2
