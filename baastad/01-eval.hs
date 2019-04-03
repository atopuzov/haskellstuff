import           Term

data M a = I a

runI :: M a -> a
runI (I a) = a

instance Functor M where
  fmap f ia = I . f $ runI ia

instance Applicative M where
  pure x = I x
  f <*> a = I $ (runI f) (runI a)

instance Monad M where
  return = pure
  ia >>= f = I $ runI $ f (runI ia)

eval :: Term -> M Int
eval (Con a)   = return a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> return (a `div` b)

runOk = runI $ eval Term.answer
runFail = runI $ eval Term.error
