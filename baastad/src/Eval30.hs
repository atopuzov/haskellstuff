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

eval' (Con a) =
  printLine (line (Con a) a)             `withLog` \_ ->
  returnLog a

eval' (Div t u) =
  eval' t                                `withLog` \a ->
  eval' u                                `withLog` \b ->
  printLine (line (Div t u) (a `div` b)) `withLog` \_ ->
  returnLog $ a `div` b

runOk' = eval' Term.answer

withLog :: M a -> (a -> M b) -> M b
withLog op k =
  let (x, a) = op
      (y, b) = k a
  in (x ++ y, b)

returnLog x = ("", x)
printLine s = (s, ())
