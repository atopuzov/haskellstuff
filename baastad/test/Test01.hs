module Test01 where
import           Eval01

import           Test.QuickCheck           (Arbitrary, arbitrary)
import           Test.QuickCheck.Classes   (lawsCheck, functorLaws, applicativeLaws, monadLaws)
import           Data.Data                 (Proxy (..))

instance (Arbitrary a) => Arbitrary (M a) where
    arbitrary = I <$> arbitrary

testMonad = do
  lawsCheck (functorLaws (Proxy :: Proxy M))
  lawsCheck (applicativeLaws (Proxy :: Proxy M))
  lawsCheck (monadLaws (Proxy :: Proxy M))
