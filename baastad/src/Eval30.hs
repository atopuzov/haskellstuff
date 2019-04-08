module Eval30 where
import           Line
import           Term

type M a = (Output, a)

eval :: Term -> M Int
eval (Con a) = (line (Con a) a, a)
eval (Div t u) =
  let (x, a) = eval t
      (y, b) = eval u in
    (x ++ y ++ line (Div t u) (a `div` b), a `div` b)

runOk = eval Term.answer
printAnswer = putStrLn . show . snd $ runOk
printLog = putStr . fst $ runOk
