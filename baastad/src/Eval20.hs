module Eval20 where
import           Term

type M a = State -> (a, State)
type State = Int

eval :: Term -> M Int
eval (Con a) x = (a, x)
eval (Div t u) x =
  let (a, y) = eval t x
      (b, z) = eval u y in
    (a `div` b, z + 1)

runOk = eval Term.answer 0
runFail = eval Term.error 0

eval' (Con a)   = returnState a
eval' (Div t u) =
  eval' t        `andThen` \a ->
  eval' u        `andThen` \b ->
  tick `andThen` \() ->
  returnState (a `div` b)

eval'' (Con a)   = returnState a
eval'' (Div t u) =
  eval'' t        `andThen` \a ->
  eval'' u        `andThen` \b ->
  getState       `andThen` \z ->
  putState (z+1) `andThen` \_ ->
  returnState (a `div` b)

runOk' = eval' Term.answer 0

andThen :: M a -> (a -> M b) -> M b
andThen op k = \x ->
  let (a, y) = op x
      (b, z) = (k a) y
  in (b, z)

tick' = \s -> ((), s + 1)

tick = getState `andThen` \s -> putState $ s + 1

returnState x = \s -> (x, s)
getState = \s -> (s, s)
putState s = \_ -> ((), s)
