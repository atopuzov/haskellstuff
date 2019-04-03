import           Term

eval :: Term -> Either String Int
eval (Con a) = return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  if b == 0
  then Left "divide by zero"
  else pure (a `div` b)

-- Desugared
-- -- eval (Div t u) = eval t >>= \a -> eval u >>= \b -> safeDiv a b

runOk = eval Term.answer
runFail = eval Term.error
