import qualified Test01
import qualified Test02
import qualified Test11
import qualified Test12
-- import qualified Test21
import qualified Test22
import qualified Test31
import qualified Test32


main :: IO ()
main = do
  Test01.testMonad
  -- Test02.testMonad

  -- putStrLn "Exceptions"
  -- Test11.testMonad
  -- Test12.testMonad

  -- putStrLn "State"
  -- -- Test21.testMonad
  -- Test22.testMonad

  -- putStrLn "Writer"
  -- Test31.testMonad
  -- Test32.testMonad
