import           Term

import           Control.Monad.State (State, get, put, runState)

eval :: Term -> State Int Int
eval (Con a) = do
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  x <- get
  put (x + 1)
  return (a `div` b)

runOk = runState (eval Term.answer) 0
runFail = runState (eval Term.error) 0 -- exception
