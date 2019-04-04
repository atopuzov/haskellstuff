import           Term

data M a = I a

runI :: M a -> a
runI (I a) = a

instance Functor M where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f ia = I . f $ runI ia

instance Applicative M where
  -- pure :: a -> f a
  pure x = I x
  -- (<*>) :: f (a -> b) -> f a -> f b
  f <*> a = I $ (runI f) (runI a)

instance Monad M where
  return = pure
  -- (>>=) m a -> (a -> m b) -> mb
  ia >>= f = f (runI ia)

eval :: Term -> M Int
eval (Con a)   = return a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> return (a `div` b)

runOk = runI $ eval Term.answer
runFail = runI $ eval Term.error
