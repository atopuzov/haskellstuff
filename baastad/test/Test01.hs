module Test01 where
import           Eval01

import           MonadLaws

import           Test.QuickCheck          (Arbitrary, arbitrary, quickCheck)
import           Test.QuickCheck.Function (Fun)


instance (Arbitrary a) => Arbitrary (M a) where
    arbitrary = I <$> arbitrary


testMonad = do
  quickCheck (prop_LeftUnit :: Int -> Fun Int (M Int) -> Bool)
  quickCheck (prop_RightUnit :: M Int -> Bool)
  quickCheck (prop_Assoc :: M Int -> Fun Int (M String) -> Fun String (M Double) -> Bool)
