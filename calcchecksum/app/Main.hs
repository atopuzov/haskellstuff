module Main where

import qualified Ver1
import qualified Ver2
import qualified Ver3
import qualified Ver4
import qualified Ver5

example :: [[Int]]
example = [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]]

main :: IO ()
main = do
  putStrLn "Version 1:"
  putStrLn $ show $ Ver1.cksum example
  putStrLn $ show $ Ver1.cksum' example

  putStrLn "Version 2:"
  putStrLn $ show $ Ver2.cksum example
  putStrLn $ show $ Ver2.cksum' example

  putStrLn "Version 3:"
  putStrLn $ show $ Ver3.cksum example

  putStrLn "Version 4:"
  putStrLn $ show $ Ver4.cksum example

  putStrLn "Version 5:"
  putStrLn $ show $ Ver5.cksum example
