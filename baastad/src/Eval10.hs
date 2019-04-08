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
