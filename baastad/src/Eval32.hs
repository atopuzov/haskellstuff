module Eval32 where
import           Line
import           Term

import           Control.Monad.Writer (Writer, runWriter, tell)


eval :: Term -> Writer Output Int
eval (Con a) = do
  tell $ line (Con a) a
  return $ a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  tell $ line (Div t u) (a `div` b)
  return $ a `div` b

runOk = runWriter (eval Term.answer)
printAnswer = putStrLn . show . fst $ runOk
printLog = putStr . snd $ runOk
