import qualified Data.Char as C
import qualified Data.Map as DM
import qualified Data.Maybe as M

-- We are given a string s and an array roll where roll[i] represents rolling
-- first roll[i] characters in string. We need to apply every roll[i] on string
-- and output final string. Rolling means increasing ASCII value of character,
-- like rolling ‘z’ would result in ‘a’, rolling ‘b’ would result in ‘c’, etc.

rollChar :: Char -> Char
rollChar = rollChar' 1

rollChar' :: Int -> Char -> Char
rollChar' n c = C.chr r
  where
    r = C.ord 'a' + (C.ord c - C.ord 'a' + n) `mod` 26

rollTheString :: String -> [Int] -> String
rollTheString str rolls = rolled
  where
    deltasM = DM.fromListWith (+) $ zip rolls $ repeat 1
    deltas = map (M.fromMaybe 0 . flip DM.lookup deltasM) [0..length str - 1]
    charRolls = tail $ scanl (-) (length rolls) deltas
    rolled = zipWith rollChar' charRolls str

main :: IO ()
main = do
  print $ rollTheString "abz" [3,2,1] == "dda"
  print $ rollTheString "abz" [3] == "bca"
  print $ rollTheString "vwxyz" [1,2,3,4,5] == "aaaaa"
  print $ rollTheString "vgxgpuamkx" [5, 5, 2, 4, 7, 6, 2, 2, 8, 7] == "fqenvydnkx"
