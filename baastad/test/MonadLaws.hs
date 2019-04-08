module MonadLaws where


import           Test.QuickCheck.Function (Fun (..))


prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_LeftUnit x (Fun _ f) =
   (return x >>= f) == f x

prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
prop_RightUnit m =
   (m >>= return) == m

prop_Assoc :: (Eq (m c), Monad m)
           => m a -> Fun a (m b) -> Fun b (m c) -> Bool
prop_Assoc m (Fun _ f) (Fun _ g) =
   ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

prop_Monad :: (Eq (m b), Eq (m a), Eq (m c), Monad m)
           => a -> m a -> Fun a (m b) -> Fun b (m c) -> Bool
prop_Monad a ma fab fbc =
  prop_LeftUnit  a fab      &&
  prop_RightUnit ma         &&
  prop_Assoc     ma fab fbc
