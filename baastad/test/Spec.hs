import qualified Test01
import qualified Test02
import qualified Test11


main :: IO ()
main = do
  Test01.testMonad
  Test02.testMonad
  Test11.testMonad
