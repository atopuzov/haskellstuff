module Eval00 where
import           Term

eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = (eval t) `div` (eval u)

runOk = eval Term.answer
runFail = eval Term.error

type M a = a

runI op k = k op

eval' (Con a) = a
eval' (Div t u) =
  eval' t `runI` \a ->
  eval' u `runI` \b ->
  a `div` b

runOk' = eval' Term.answer
