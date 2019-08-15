module Ver1
where
import Data.Maybe (catMaybes)

rowCksum :: (Num a, Ord a) => [a] -> a
rowCksum xs = rmax - rmin
  where
    rmin = minimum xs
    rmax = maximum xs

cksum :: (Num a, Ord a) => [[a]] -> a
cksum = sum . map rowCksum

rowCksum' :: (Num a, Ord a) => [a] -> Maybe a
rowCksum' [] = Nothing
rowCksum' xs = Just $ rmax - rmin
  where
    rmin = minimum xs
    rmax = maximum xs

-- filterNothing xs = foldr
--   (\x a -> case x of
--             Just v -> v:a
--             Nothing -> a
--   ) [] xs
filterNothing = catMaybes

cksum' :: (Num a, Ord a) => [[a]] -> Maybe a
cksum' [] = Nothing
cksum' xs = Just . sum . filterNothing $ map rowCksum' xs
