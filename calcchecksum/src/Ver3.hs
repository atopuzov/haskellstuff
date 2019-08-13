module Ver3
where

import Data.Semigroup (Min(..), Max(..), Sum(..), getMin, getMax, getSum, Semigroup, (<>))

data MinMax a = MinMax {
    myMin :: Min a
  , myMax :: Max a
  } deriving Show

instance Ord a => Semigroup (MinMax a) where
  a <> b = MinMax (myMin a <> myMin b) (myMax a <> myMax b)

mkMinMax :: (Num a, Ord a) => a -> MinMax a
mkMinMax x = MinMax (Min x) (Max x)

calcDiff :: Num a => MinMax a -> a
calcDiff x = (getMax . myMax $ x) - (getMin . myMin $ x)

rowCksum :: (Num a, Ord a) => [a] -> Maybe (Sum a)
rowCksum xs = fmap (Sum . calcDiff) res
  where
    res = foldMap (Just . mkMinMax) xs

cksum :: (Num a, Ord a) => [[a]] -> Maybe a
cksum xs = fmap getSum $ foldMap (rowCksum) xs
