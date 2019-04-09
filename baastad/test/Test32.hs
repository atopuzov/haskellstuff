{-# LANGUAGE FlexibleInstances #-}

module Test32 where
import           Eval32

import           MonadLaws

import           Control.Monad.Writer     (Writer, writer)
import           Test.QuickCheck          (Arbitrary, arbitrary, quickCheck)
import           Test.QuickCheck.Function (Fun)


instance (Monoid o, Arbitrary o, Arbitrary a) => Arbitrary (Writer o a) where
    arbitrary = do
      o <- arbitrary
      a <- arbitrary
      return $ writer (o, a)


testMonad = do
  quickCheck (prop_LeftUnit :: Int -> Fun Int (Writer String Int) -> Bool)
  quickCheck (prop_RightUnit :: Writer String Int -> Bool)
  quickCheck (prop_Assoc :: Writer String Int -> Fun Int (Writer String String) -> Fun String (Writer String Double) -> Bool)
