import           Term

eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = (eval t) `div` (eval u)

runOk = eval Term.answer
runFail = eval Term.error
