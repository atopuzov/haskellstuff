import           Line
import           Term

data M a = M (Output, a) deriving Show

runM :: M a -> (String, a)
runM (M x) = x

instance Functor M where
  fmap f w = M $ let (out, x) = runM w in
    (out, f x)

instance Applicative M where
  pure x = M ("", x)
  wf <*> wx = let (l1, f) = runM wf
                  (l2, x) = runM wx in
              M (l1 ++ l2, f x)

instance Monad M where
  return = pure
  wx >>= f =
    let (l1, x) = runM wx
        (l2, y) = runM (f x) in
      M (l1 ++ l2, y)

tell :: Output -> M ()
tell s = M (s, ())

eval :: Term -> M Int
eval (Con a) = do
  tell $ line (Con a) a
  return a
eval (Div t u) = do
  a <- eval t
  b <- eval u
  tell $ line (Div t u) (a `div` b)
  return $ a `div` b

runOk = runM (eval Term.answer)
printAnswer = putStrLn . show . snd $ runOk
printLog = putStr . fst $ runOk
