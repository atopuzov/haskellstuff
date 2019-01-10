{-# LANGUAGE FlexibleContexts #-}
-- FelixbleContexts required for MonadState

import Control.Monad.State (MonadState, State, get, put, runStateT, evalStateT, execStateT)
import Data.Functor.Identity (Identity, runIdentity)

-- Threading the state trough functions and returning vale and state product
inc1 :: Int -> (Int, Int)
inc1 state = (value, newState)
  where
    value = state
    newState = state + 1

mul10 :: Int  -> (Int, Int)
mul10 state = (value, newState)
  where
    value = state
    newState = state * 10

inc1mul10 :: Int -> ((Int, Int), Int)
inc1mul10 state = (value, finalState)
  where
    (incVal, state') = inc1 state
    (mulVal, finalState) = mul10 state'
    value = (incVal, mulVal)

runInc1Mul10 = inc1mul10 10

-- Using custom state
data S s a = S (s -> (a, s))

instance Functor (S s) where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> S s a -> S s b
  -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
  fmap f (S stateFunc) = S $ \startState ->
    let (value, newState) = stateFunc startState
    in (f value, newState)

instance Applicative (S s) where
  -- pure :: a -> f a
  -- pure :: a -> S s a
  -- pure :: a -> (s -> (a, s))
  pure a = S $ \s -> (a, s)
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: S s (a -> b) -> S s a -> S s b
  -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
  (S stateFuncA) <*> (S stateFuncB) = S $ \startState ->
    let (function, newState) = stateFuncA startState
        (value,  finalState) = stateFuncB newState
    in (function value, finalState)

instance Monad (S s) where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: S s a -> (a -> S s b) -> S s b
  -- (>>=) :: (s -> (a, s)) -> (a -> s -> (a, s)) -> (s -> (b, s))
  (S stateFunc) >>= f = S $ \startState ->
    let (value, newState) = stateFunc startState
        (S stateFuncB) = f value
    in stateFuncB newState

runS :: S s a -> s -> (a, s)
runS (S s) a = s a

getS :: S s s
getS = S $ \s -> (s, s)

putS :: s -> S s ()
-- putS s = S $ \_ -> ((), s)
putS s = S $ const ((), s)

inc1S :: S Int Int
inc1S = do
  x <- getS
  putS $ x + 1
  return x

mul10S :: S Int Int
mul10S = do
  x <- getS
  putS $ x * 10
  return x

inc1mul10S :: S Int (Int, Int)
inc1mul10S = do
  x1 <- inc1S
  x2 <- mul10S
  return (x1, x2)

runInc1Mul10S = (runS inc1mul10S) 10

-- Using newtype
newtype Sn s a = Sn { runSn :: s -> (a, s) }

instance Functor (Sn s) where
  fmap f x = Sn $ \s0 ->
    let (a, s1) = (runSn x) s0
    in (f a, s1)

instance Applicative (Sn s) where
  pure a = Sn $ \s -> (a, s)
  sa <*> sb = Sn $ \s0 ->
    let (fn, s1) = (runSn sa) s0
        (a,  s2) = (runSn sb) s1
    in (fn a, s2)

instance Monad (Sn s) where
  return = pure
  x >>= f = Sn $ \s0 ->
    let (a, s1) = (runSn x) s0
        sb = f a
    in runSn sb s1

getSn :: Sn s s
getSn = Sn $ \s -> (s, s)

putSn :: s -> Sn s ()
putSn s = Sn $ const ((), s)

inc1Sn :: Sn Int Int
inc1Sn = do
  x <- getSn
  putSn $ x + 1
  return x

mul10Sn :: Sn Int Int
mul10Sn = do
  x <- getSn
  putSn $ x * 10
  return x

inc1mul10Sn :: Sn Int (Int, Int)
inc1mul10Sn = do
  x1 <- inc1Sn
  x2 <- mul10Sn
  return (x1, x2)

runInc1Mul10Sn = (runSn inc1mul10Sn) 10

-- Using State monad (StateT Identity)
inc1ST :: State Int Int
inc1ST = do
  x <- get
  put $ x + 1
  return x

mul10ST :: State Int Int
mul10ST = do
  x <- get
  put $ x * 10
  return x

inc1mul10ST :: State Int (Int, Int)
inc1mul10ST = do
  x1 <- inc1ST
  x2 <- mul10ST
  return (x1, x2)

runInc1Mul10ST :: Identity ((Int, Int), Int)
runInc1Mul10ST = (runStateT inc1mul10ST) 10

-- Using MonadState
inc1MS :: (MonadState Int m) => m Int
inc1MS = do
  x <- get
  put $ x + 1
  return x

mul10MS :: (MonadState Int m) => m Int
mul10MS = do
  x <- get
  put $ x * 10
  return x

inc1mul10MS :: (MonadState Int m) => m (Int, Int)
inc1mul10MS = do
  x1 <- inc1MS
  x2 <- mul10MS
  return (x1, x2)

runInc1Mul10MS :: Identity ((Int, Int), Int)
runInc1Mul10MS = (runStateT inc1mul10MS) 10

main :: IO ()
main = do
  putStrLn "threading state and value state product"
  let (val, st) = runInc1Mul10
  putStrLn $ "Value: " ++ show val
  putStrLn $ "State: " ++ show st

  putStrLn "using custom state"
  let (valS, stS) = runInc1Mul10S
  putStrLn $ "Value: " ++ show valS
  putStrLn $ "State: " ++ show stS

  putStrLn "using custom state with newtype"
  let (valSn, stSn) = runInc1Mul10Sn
  putStrLn $ "Value: " ++ show valSn
  putStrLn $ "State: " ++ show stSn

  putStrLn "using State Monad"
  let (valST, stST) = runIdentity runInc1Mul10ST
  putStrLn $ "Value: " ++ show valST
  putStrLn $ "State: " ++ show stST

  putStrLn "using MonadState"
  let (valMS, stMS) = runIdentity runInc1Mul10MS
  putStrLn $ "Value: " ++ show valMS
  putStrLn $ "State: " ++ show stMS
