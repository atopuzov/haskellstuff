import qualified Data.Map as DM

solution :: String -> Bool
solution s = (length odds) <= 1
  where
    letterCount = DM.fromListWith (+) $ zip s $ repeat 1
    odds = filter (not . even) $ DM.elems letterCount

main :: IO ()
main = do
  print $ solution("AABBCC") == True
  print $ solution("AABBC") == True
  print $ solution("ABBC") == False
  print $ solution("AAABBBCCCDDD") == False
  print $ solution("AABB") == True
