module Ver4
where

import Data.Semigroup (Min(..), Max(..), Sum(..), getMin, getMax, getSum, Semigroup)
import Data.Bifunctor (bimap)

mkMinMax :: a -> (Maybe (Min a), Maybe (Max a))
mkMinMax x = (Just $ Min x, Just $ Max x)

calcDiff :: (Applicative f, Num a) => (f a, f a) -> f a
calcDiff x = (-) <$> (snd x) <*> (fst x)

rowCksum :: (Num a, Ord a) => [a] -> Maybe (Sum a)
rowCksum xs = fmap Sum $ calcDiff res
  where
    res = bimap (fmap getMin) (fmap getMax) $ foldMap mkMinMax xs

cksum :: (Num a, Ord a) => [[a]] -> Maybe a
cksum xs = fmap getSum $ foldMap (rowCksum) xs
