import qualified Data.List as DL

solution s = longestSubstring
  where
    suffixes = DL.sort $ map (flip drop s) [0..length(s) - 1]
    suffixPairs = zip suffixes $ tail suffixes
    longestSubstrings = fmap (fmap fst . filter (\(a, b) -> a == b) . \(a, b) -> zip a b) suffixPairs
    longestSubstring = foldr (\x y -> if length(x) > length(y) then x else y) "" longestSubstrings


main = do
  print $ solution("ABCDEFGABCEF") == "ABC"
  print $ solution("GEEKSFORGEEKS") == "GEEKS"
