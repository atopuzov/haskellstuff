module Eval02 where
import           Term

import           Data.Functor.Identity


eval :: Term -> Identity Int
eval (Con a)   = return a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> return (a `div` b)

-- eval (Div t u) = do
--   a <- eval t
--   b <- eval u
--   return $ a `div` b

runOk = runIdentity $ eval Term.answer
runFail = runIdentity $ eval Term.error
