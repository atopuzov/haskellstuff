import           Term

type MS a = State -> (a, State)
type State = Int

eval :: Term -> MS Int
eval (Con a) x = (a, x)
eval (Div t u) x =
  let (a, y) = eval t x
      (b, z) = eval u y in
    (a `div` b, z + 1)

runOk = eval Term.answer 0
runFail = eval Term.error 0
