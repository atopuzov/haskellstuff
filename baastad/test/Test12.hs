module Test12 where
import           Eval12

import           Test.QuickCheck          (quickCheck)
import           Test.QuickCheck.Function (Fun)
import           Test.QuickCheck.Classes  (lawsCheck, functorLaws, applicativeLaws, monadLaws)
import           Data.Data                (Proxy (..))


testMonad = do
  lawsCheck (functorLaws (Proxy :: Proxy (Either String)))
  lawsCheck (applicativeLaws (Proxy :: Proxy (Either String)))
  lawsCheck (monadLaws (Proxy :: Proxy (Either String)))
