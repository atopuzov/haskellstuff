module Ver3
where

import Data.Semigroup (Min(..), Max(..), Sum(..), getMin, getMax, getSum, Semigroup, (<>))
import Data.Semigroup.Foldable (foldMap1)
import Data.List.NonEmpty (NonEmpty((:|)))

data MinMax a = MinMax (Min a) (Max a) deriving Show

instance Ord a => Semigroup (MinMax a) where
  MinMax a b <> MinMax c d = MinMax (a <> c) (b <> d)

mkMinMax :: (Num a, Ord a) => a -> MinMax a
mkMinMax x = MinMax (Min x) (Max x)

rowCksum :: (Num a, Ord a) => [a] -> Maybe (Sum a)
rowCksum [] = Nothing
rowCksum (x:xs) = Just . Sum $ (getMax b) - (getMin a)
  where
    MinMax a b = foldMap1 mkMinMax (x :| xs)

cksum :: (Num a, Ord a) => [[a]] -> Maybe a
cksum [] = Nothing
cksum (x:xs)= fmap getSum $ foldMap1 (rowCksum) (x :| xs)
