module Main where

import qualified Ver1
import qualified Ver2
import qualified Ver3

main :: IO ()
main = do
  let sh = [[1,2,4,5], [6,8,2], [5,6,7]]
  putStrLn $ show $ Ver1.cksum sh
  putStrLn $ show $ Ver1.cksum' sh
  putStrLn $ show $ Ver2.cksum sh
  putStrLn $ show $ Ver2.cksum' sh
  putStrLn $ show $ Ver3.cksum sh
