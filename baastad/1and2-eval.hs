import Term
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans.Class (lift)


eval :: Term -> StateT Int (Either String) Int
eval (Con a) = do
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  x <- get
  put (x + 1)
  lift $ if b == 0
         then Left "divide by zero"
         else Right (a `div` b)

runOk =   runStateT (eval Term.answer) 0
runFail = runStateT (eval Term.error)  0
