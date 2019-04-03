import           Line
import           Term

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.State  (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)


eval :: Term -> WriterT Output (StateT Int (Either String)) Int
eval (Con a) = do
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  x <- lift get
  lift $ put (x + 1)
  tell $ line (Div t u) (a `div` b)
  lift . lift $ if b == 0
                then Left "divide by zero"
                else Right (a `div` b)

runOk =   runStateT (runWriterT (eval (Term.answer))) 0
runFail = runStateT (runWriterT (eval (Term.error)))  0

eval' :: Term -> StateT Int (WriterT Output (Either String)) Int
eval' (Con a) = do
  return a
eval' (Div t u) = do
  a <- eval' t
  b <- eval' u
  x <- get
  put (x + 1)
  lift . tell $ line (Div t u) (a `div` b)
  lift . lift $ if b == 0
                then Left "divide by zero"
                else Right (a `div` b)

runOk' =   runWriterT (runStateT (eval' Term.answer) 0)
runFail' = runWriterT (runStateT (eval' Term.error)  0)
