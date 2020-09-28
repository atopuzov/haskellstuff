module Test11 where
import           Eval11

import           Test.QuickCheck          (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen      (oneof)
import           Test.QuickCheck.Classes  (lawsCheck, functorLaws, applicativeLaws, monadLaws)
import           Data.Data                (Proxy (..))


instance (Arbitrary a) => Arbitrary (M a) where
    arbitrary = oneof
      [Raise <$> arbitrary, Return <$> arbitrary]


testMonad = do
  lawsCheck (functorLaws (Proxy :: Proxy M))
  lawsCheck (applicativeLaws (Proxy :: Proxy M))
  lawsCheck (monadLaws (Proxy :: Proxy M))
