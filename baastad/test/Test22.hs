{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Test22 where
import           Eval22

import           Control.Monad.State      (State (..), get, put, runState,
                                           state)
import           MonadLaws

import           Test.QuickCheck          (Arbitrary, arbitrary, quickCheck)
import           Test.QuickCheck.Function (Fun (..))
import           Test.QuickCheck.Gen      (oneof)

import           Text.Show.Functions


instance Show (State s a) where
  show st = show (runState st)


instance (Eq s, Num s, Eq a) => Eq (State s a) where
  sa == sb = a == b && as == bs
    where
      a = fst $ runState sa $ 0
      as = snd $ runState sa $ 0
      b = fst $ runState sb $ 0
      bs = snd $ runState sb $ 0


instance (Arbitrary (Fun s s), Arbitrary a) => Arbitrary (State s a) where
  arbitrary = do
    a <- arbitrary
    (Fun _ f) <- arbitrary
    return $ state (\s -> (a, f s))


testMonad = do
  quickCheck (prop_LeftUnit :: Int -> Fun Int (State Int Int) -> Bool)
  quickCheck (prop_RightUnit :: State Int Int -> Bool)
  quickCheck (prop_Assoc :: State Int Int -> Fun Int (State Int String) -> Fun String (State Int Double) -> Bool)
