module Eval10 where
import           Term

data M a = Raise Exception
  | Return a deriving Show

type Exception = String

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) =
  case eval t of
    Raise e -> Raise e
    Return a ->
      case eval u of
        Raise e -> Raise e
        Return b ->
          if b == 0
          then Raise "divide by zero"
          else Return (a `div` b)

runOk = eval Term.answer
runFail = eval Term.error

safeDiv a 0 = Raise "divide by zero"
safeDiv a b = Return $ a `div` b

returnOk = Return
returnFail = Raise

ifOk :: M a -> (a -> M b) -> M b
ifOk op k = case op of
  Raise e -> Raise e
  Return a -> k a

eval' (Con a)  = Return a
eval' (Div t u) =
  ifOk (eval' t) (\a ->
  ifOk (eval' u) (\b ->
  safeDiv a b))

eval'' (Con a)  = Return a
eval'' (Div t u) =
  eval' t `ifOk` \a ->
  eval' u `ifOk` \b ->
  safeDiv a b

runOk' = eval' Term.answer
runFail' = eval' Term.error
