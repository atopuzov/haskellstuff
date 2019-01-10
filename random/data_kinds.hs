{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol)
import Data.Ratio ((%))

newtype Money (currency :: Symbol) = Money Rational deriving Show

add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)

fivePence :: Money "GBP"
fivePence = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros = Money 2
