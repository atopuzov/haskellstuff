import           Term

data M a = Raise Exception
  | Return a deriving Show
type Exception = String

instance Functor M where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Return a) = Return (f a)
  fmap _ (Raise s)  = Raise s

instance Applicative M where
  -- pure :: a -> f a
  pure = Return
  -- (<*>) :: f (a -> b) -> f a -> f b
  (Return f) <*> (Return a) = Return (f a)
  _ <*> (Raise s) = Raise s

instance Monad M where
  return = pure
  -- (>>=) m a -> (a -> m b) -> m b
  (Return a) >>= f = case f a of
    Return fa -> Return fa
    Raise s   -> Raise s
  (Raise s) >>= f = Raise s
  fail = Raise

eval :: Term -> M Int
eval (Con a) = pure a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  if b == 0
    then fail "divide by zero"
    else return (a `div` b)

runOk = eval Term.answer
runFail = eval Term.error
