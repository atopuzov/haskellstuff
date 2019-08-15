module Ver2
where
import Data.Maybe (catMaybes)

findMinMax (x:xs) = foldr helper (x, x) xs
  where
    helper x (cmin, cmax) =
      let nmin = if x < cmin then x else cmin
          nmax = if x > cmax then x else cmax in
        (nmin, nmax)

rowCksum :: (Num a, Ord a) => [a] -> a
rowCksum xs = rmax - rmin
  where
    (rmin, rmax) = findMinMax xs

cksum :: (Num a, Ord a) => [[a]] -> a
cksum xs = sum $ map rowCksum xs

rowCksum' [] = Nothing
rowCksum' xs = Just $ rmax - rmin
  where
    (rmin, rmax) = findMinMax xs

cksum' [] = Nothing
cksum' xs = Just . sum . catMaybes $ map rowCksum' xs


findMinMax' [] = Nothing
findMinMax' (x:xs) = Just $ foldr (\x (cmin, cmax) -> helper x cmin cmax) (x, x) xs
  where
    helper x cmin cmax =
      let nmin = if x < cmin then x else cmin
          nmax = if x > cmax then x else cmax in
        (nmin, nmax)

rowCksum'' xs = case findMinMax' xs of
  Nothing -> Nothing
  Just (mmin, mmax) -> Just $ mmax - mmin
