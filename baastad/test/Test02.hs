module Test02 where
import           Eval02

import           Data.Functor.Identity
import           MonadLaws

import           Test.QuickCheck          (quickCheck)
import           Test.QuickCheck.Function (Fun)


testMonad = do
  quickCheck (prop_LeftUnit :: Int -> Fun Int (Identity Int) -> Bool)
  quickCheck (prop_RightUnit :: Identity Int -> Bool)
  quickCheck (prop_Assoc :: Identity Int -> Fun Int (Identity String) -> Fun String (Identity Double) -> Bool)
