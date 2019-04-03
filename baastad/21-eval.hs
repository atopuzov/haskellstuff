import           Term

data M a = ST (State -> (a, State))
type State = Int

runM :: M a -> State -> (a, State)
runM (ST st) = \s -> st s

instance Functor M where
  fmap f (ST st) = ST $
    \s -> let (x, s') = st s in
      (f x, s')

instance Applicative M where
  pure x = ST $ \s -> (x, s)
  sf <*> sa = ST $
    \s -> let (f, s') = runM sf s
              (x, s'') = runM sa s' in
           (f x, s'')

instance Monad M where
  return = pure
  sa >>= f = ST $
    \s -> let (x, s') = runM sa s
              (y, s'') = runM (f x) s' in
           (y, s'')

get :: M State
get = ST $ \s -> (s, s)

put :: State -> M ()
put s = ST $ \_ -> ((), s)

tick :: M ()
tick = get >>= \x -> put (x + 1)

eval :: Term -> M Int
eval (Con a) = do
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  tick
  return (a `div` b)

runOk = runM (eval Term.answer) 0
runFail = runM (eval Term.error) 0
