{-# LANGUAGE FlexibleContexts #-}
module Eval1and2and3 where
import           Line
import           Term

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.State  (StateT, get, put, runStateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import           Control.Monad.Except       (MonadError, throwError)
import qualified Control.Monad.State        as MS (MonadState, get, put)
import qualified Control.Monad.Writer       as MW (MonadWriter, tell)



tick = get >>= put . (+1)

eval :: Term -> WriterT Output (StateT Int (Either String)) Int
eval (Con a) = do
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  lift tick
  tell $ line (Div t u) (a `div` b)
  lift . lift $ if b == 0
                then Left "divide by zero"
                else Right (a `div` b)

runOk =   runStateT (runWriterT (eval (Term.answer))) 0
runFail = runStateT (runWriterT (eval (Term.error)))  0


tick' = get >>= put . (+1)

eval' :: Term -> StateT Int (WriterT Output (Either String)) Int
eval' (Con a) = do
  return  a
eval' (Div t u) = do
  a <- eval' t
  b <- eval' u
  tick'
  lift . tell $ line (Div t u) (a `div` b)
  lift . lift $ if b == 0
                then Left "divide by zero"
                else Right (a `div` b)

runOk' =   runWriterT (runStateT (eval' Term.answer) 0)
runFail' = runWriterT (runStateT (eval' Term.error)  0)


tick'' :: (MS.MonadState Int m) => m ()
tick'' = MS.get >>= MS.put . (+1)

eval'':: (MW.MonadWriter Output m, MS.MonadState Int m, MonadError String m) => Term -> m Int
eval'' (Con a) = return a
eval'' (Div t u) = do
  a <- eval'' t
  b <- eval'' u
  tick''
  MW.tell $ line (Div t u) (a `div` b)
  if b == 0
    then throwError "divide by zero"
    else return (a `div` b)

type T1 = WriterT Output (StateT Int (Either String)) Int
type T2 = StateT Int (WriterT Output (Either String)) Int

runOkT1 = runStateT  (runWriterT ((eval'' Term.answer)::T1)) 0
runOkT2 = runWriterT (runStateT  ((eval'' Term.answer)::T2)  0)
