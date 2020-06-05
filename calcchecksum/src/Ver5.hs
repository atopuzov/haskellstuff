{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
module Ver5
where
import Control.Applicative (Applicative, (<*>), pure)
import Data.Functor (Functor, fmap, (<$>))
import Data.Monoid (Monoid, Sum(..), getSum, mempty)
import Data.Semigroup ((<>), Max(..), getMax, Min(..), getMin)
import Data.Function (const, (.), ($))
import Prelude ((-))
import Data.Int (Int)
import Data.List (foldl)

-- https://youtu.be/6a5Ti0r8Q2s
-- https://github.com/ekmett/folds (Data.Fold.M)
data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)

instance Functor (Fold i) where
  fmap k (Fold tally summarize) = Fold tally (k . summarize)

instance Applicative (Fold i) where
  pure o = Fold (const ()) (const o)
  Fold tallyF summarizeF <*> Fold tallyX summarizeX = Fold tally summarize
    where
      tally i = (tallyF i, tallyX i)
      summarize (mF, mX) = summarizeF mF (summarizeX mX)

fold' (Fold tally summarize) is = summarize $ reduce $ fmap tally is
  where
    reduce = foldl (<>) mempty

sum' = Fold Sum getSum

max' :: Fold Int Int
max' = Fold Max getMax

min' :: Fold Int Int
min' = Fold Min getMin

calcDiff :: Fold Int Int
calcDiff = (-) <$> max' <*> min'

rowCksum :: [Int] -> Int
rowCksum = fold' calcDiff

clc = Fold (Sum . rowCksum) getSum

cksum :: [[Int]] -> Int
cksum = fold' clc
