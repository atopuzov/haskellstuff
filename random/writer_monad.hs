{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer (MonadWriter, Writer, tell, runWriter, runWriterT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Monoid (mempty)


-- manual threading of log
logSum :: [(Int, Int)] -> Int -> Int -> (Int, [(Int, Int)])
logSum log x y = (result, newlog)
  where
    result = x + y
    newlog = log <> [(x, y)]

calc :: [(Int, Int)] -> ((Int, Int), [(Int, Int)])
calc log = (result, newlog)
  where
    result = (x, y)
    (x, logx) = logSum log 1 2
    (y, logy) = logSum log 2 10
    newlog = log <> logx <> logy

runCalc :: ((Int, Int), [(Int, Int)])
runCalc = calc []

-- custom writer
data W w a = W (a, w)

instance Functor (W w) where
  fmap f (W (a, w)) = W (f a, w)

instance Monoid w => Applicative (W w) where
  -- pure :: a -> f a
  -- pure :: a -> W w a
  -- pure :: a -> (a, w)
  pure a = W (a, mempty)
  -- (<*>) :: f (a -> b) -> f a -> f b
  (W (f, w1)) <*> (W (x, w2)) = W (f x, w1 <> w2)

instance Monoid w => Monad (W w) where
  return = pure
  -- >>= :: m a -> (a -> m b) -> m b
  -- >>= :: W w a -> (a -> W w b) -> W w b
  -- >>= :: (w, a) -> (a -> (w, b)) -> (w, b)
  (W (x, w1)) >>= f = W (out, w1 <> w2)
    where
      W (out, w2) = f x

runW :: W w a -> (a, w)
runW (W x) = x

tellW :: Monoid w => w -> W w ()
tellW w = W ((), w)

logSumW :: Int -> Int -> W [(Int, Int)] Int
logSumW x y = do
  tellW [(x, y)]
  return $ x + y

calcW :: W [(Int, Int)] (Int, Int)
calcW = do
  x <- logSumW 1 2
  y <- logSumW 2 10
  return (x, y)

runCalcW :: ((Int, Int), [(Int, Int)])
runCalcW = (runW calcW)

-- newtype
newtype Wn w a = Wn { runWn :: (a, w) }

instance Functor (Wn w) where
  fmap f w = Wn (f a, w')
    where
      (a, w') = runWn w

instance Monoid w => Applicative (Wn w) where
  pure a = Wn (a, mempty)
  -- (<*>) :: f (a -> b) -> f a -> f b
  w1 <*> w2 = Wn (f x, l1 <> l2)
    where
      (f, l1) = runWn w1
      (x, l2) = runWn w2

instance Monoid w => Monad (Wn w) where
  return = pure
  w1 >>= f = Wn (out, l1 <> l2)
    where
      (x, l1) = runWn w1
      (out, l2) = runWn $ f x

tellWn :: Monoid w => w -> Wn w ()
tellWn w = Wn ((), w)

logSumWn :: Int -> Int -> Wn [(Int, Int)] Int
logSumWn x y = do
  tellWn [(x, y)]
  return $ x + y

calcWn :: Wn [(Int, Int)] (Int, Int)
calcWn = do
  x <- logSumWn 1 2
  y <- logSumWn 2 10
  return (x, y)

runCalcWn :: ((Int, Int), [(Int, Int)])
runCalcWn = (runWn calcWn)

-- Writer (WriterT Identity)
logSumWM :: Int -> Int -> Writer [(Int, Int)] Int
logSumWM x y = do
  tell [(x, y)]
  return $ x + y

calcWM :: Writer [(Int, Int)] (Int, Int)
calcWM = do
  x <- logSumWM 1 2
  y <- logSumWM 2 10
  return (x, y)

runCalcWM :: ((Int, Int), [(Int, Int)])
runCalcWM = (runWriter calcWM)

-- MonadWriter
logSumMW :: MonadWriter [(Int, Int)] m => Int -> Int -> m Int
logSumMW x y = do
  tell [(x, y)]
  return $ x + y

calcMW :: MonadWriter [(Int, Int)] m => m (Int, Int)
calcMW = do
  x <- logSumMW 1 2
  y <- logSumMW 2 10
  return (x, y)

runCalcMW :: Identity ((Int, Int), [(Int, Int)])
runCalcMW = (runWriterT calcMW)

main :: IO ()
main = do
  putStrLn "threading log trough functions"
  let (out, log) = runCalc
  putStrLn $ "Output: " ++ show out
  putStrLn $ "Log: " ++ show log

  putStrLn "using custom writer"
  let (outW, logW) = runCalcW
  putStrLn $ "Output: " ++ show outW
  putStrLn $ "Log: " ++ show logW

  putStrLn "using custom writer with newtype"
  let (outWn, logWn) = runCalcWn
  putStrLn $ "Output: " ++ show outWn
  putStrLn $ "Log: " ++ show logWn

  putStrLn "using Writer monad"
  let (outWM, logWM) = runCalcWM
  putStrLn $ "Output: " ++ show out
  putStrLn $ "Log: " ++ show log

  putStrLn "using MonadState"
  let (outMW, logMW) = runIdentity runCalcMW
  putStrLn $ "Output: " ++ show outMW
  putStrLn $ "Log: " ++ show logMW
