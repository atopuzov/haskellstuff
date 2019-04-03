import           Term

data M a = Raise Exception
  | Return a deriving Show
type Exception = String

instance Functor M where
  fmap f (Return a) = Return (f a)
  fmap _ (Raise s)  = Raise s

instance Applicative M where
  pure x = Return x
  (Return f) <*> (Return a) = Return (f a)
  _ <*> (Raise s) = Raise s

instance Monad M where
  return x = Return x
  (Return a) >>= f = case f a of
    Return fa -> Return fa
    Raise s   -> Raise s
  (Raise s) >>= f = Raise s

eval :: Term -> M Int
eval (Con a) = pure a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  if b == 0
    then Raise "divide by zero"
    else return (a `div` b)

runOk = eval Term.answer
runFail = eval Term.error