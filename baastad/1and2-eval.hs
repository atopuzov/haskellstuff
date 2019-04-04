import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.State  (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Term


tick :: StateT Int (Either String) ()
tick = get >>= put . (+1)

eval :: Term -> StateT Int (Either String) Int
eval (Con a) = do
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  tick
  lift $ if b == 0
         then Left "divide by zero"
         else Right (a `div` b)

runOk =   runStateT (eval Term.answer) 0
runFail = runStateT (eval Term.error)  0
