module Eval22 where
import           Term

import           Control.Monad.State (State, get, put, runState)

tick :: State Int ()
tick = get >>= put . (+1)

eval :: Term -> State Int Int
eval (Con a) = return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  tick
  return (a `div` b)

runOk = runState (eval Term.answer) 0
runFail = runState (eval Term.error) 0 -- exception
