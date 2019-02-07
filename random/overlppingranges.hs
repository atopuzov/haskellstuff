import qualified Data.List as DL

helper [] x = [x]
helper a@((s, e):rest) b@(s', e') =
  if s' <= e
  then (s, e''):rest
  else (b:a)
  where
    e'' = max e e'

solution :: Ord a => [(a, a)] -> [(a, a)]
solution lst = reverse $ foldl helper [] slst
  where
    slst = DL.sort lst

helper' x [] = [x]
helper' a@(s, e) b@((s', e'):rest)=
  if s' <= e
  then (s, e''):rest
  else (a:b)
  where
    e'' = max e e'

solution' :: Ord a => [(a, a)] -> [(a, a)]
solution' lst = foldr helper' [] slst
  where
    slst = DL.sort lst

main :: IO ()
main = do
  print $ solution [(10, 20), (7, 9), (1, 4), (3, 5)] == [(1, 5), (7, 9), (10, 20)]
  print $ solution [(10, 20), (7, 9), (1, 4), (3, 2)] == [(1, 4), (7, 9), (10, 20)]
  print $ solution' [(10, 20), (7, 9), (1, 4), (3, 5)] == [(1, 5), (7, 9), (10, 20)]
  print $ solution' [(10, 20), (7, 9), (1, 4), (3, 2)] == [(1, 4), (7, 9), (10, 20)]
  print $ solution' [(10, 20), (7, 9), (1, 4), (3, 2), (1, 1)] == [(1, 4), (7, 9), (10, 20)]
  print $ solution [(6,8), (1,9), (2,4), (4,7)] == [(1, 9)]
  print $ solution' [(6,8), (1,9), (2,4), (4,7)] == [(1, 9)]
