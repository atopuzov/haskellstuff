module Test02 where
import           Eval02

import           Data.Functor.Identity
import           Test.QuickCheck.Classes  (lawsCheck, functorLaws, applicativeLaws, monadLaws)
import           Data.Data                (Proxy (..))

testMonad = do
  lawsCheck (functorLaws (Proxy :: Proxy Identity))
  lawsCheck (applicativeLaws (Proxy :: Proxy Identity))
  lawsCheck (monadLaws (Proxy :: Proxy Identity))
