module Test12 where
import           Eval12

import           MonadLaws

import           Test.QuickCheck          (quickCheck)
import           Test.QuickCheck.Function (Fun)


testMonad = do
  quickCheck (prop_LeftUnit :: Int -> Fun Int (Either String Int) -> Bool)
  quickCheck (prop_RightUnit :: Either String Int -> Bool)
  quickCheck (prop_Assoc :: Either String Int -> Fun Int (Either String String) -> Fun String (Either String Double) -> Bool)
