{-# LANGUAGE FlexibleContexts #-}
-- FelixbleContexts required for MonadReader
-- Based on https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html

import Control.Monad.Reader (ask, runReader, MonadReader, Reader)
import Control.Applicative ((<*>))
import Control.Monad ((>>=))

-- Environment type
type MyEnv = String

-- Threading the parametar
tom :: MyEnv -> String
tom q = q ++ " This is Tom."

jerry :: MyEnv -> String
jerry q = q ++ " This is Jerry."

tomAndJerry :: MyEnv -> String
tomAndJerry q = t ++ "\n" ++ j
  where
    t = tom q
    j = jerry q

runJerryRun :: String
runJerryRun = tomAndJerry "Who is this?"

-- Using custom Reader R
data R r a = R (r -> a)

instance Functor (R r) where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap f (R x) = R $ \a -> (f . x) a

instance Applicative (R r) where
  -- pure :: a -> f a
  -- pure :: a -> (r -> a)
  -- pure x = R $ \_ -> x
  -- pure x = R $ const x
  pure = R . const
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*)) :: (r -> a -> b) -> (r -> a) -> (r -> b)
  -- (R f) <*> (R g) = R $ \a -> (f a) (g a)
  f <*> g = R $ \a -> (runR f a) (runR g a)

-- x >>= f  = \e -> f (x e) e (for (->) e)
instance Monad (R r) where
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  -- x >>= f = R $ \a -> runR (f (runR x a)) a
  (R x) >>= f = R $ \a -> runR (f (x a)) a

runR :: R r a -> r -> a
-- runR (R f) a = f a
runR (R f) = f

askR :: R a a
-- askR = R $ \a -> a
askR = R id

tomR :: R MyEnv String
tomR = do
    env <- askR
    return (env ++ " This is Tom.")

jerryR :: R MyEnv String
jerryR = do
  env <- askR
  return (env ++ " This is Jerry.")

tomAndJerryR :: R MyEnv String
tomAndJerryR = do
    t <- tomR
    j <- jerryR
    return (t ++ "\n" ++ j)

runJerryRunR :: String
runJerryRunR = (runR tomAndJerryR) "Who is this?"

-- Using newtype
newtype Rn r a = Rn { runRn :: r -> a }

instance Functor (Rn r) where
  fmap f x = Rn $ \a -> f $ (runRn x a)

instance Applicative (Rn r) where
  -- pure x = Rn $ \_ -> x
  pure = Rn . const
  f <*> x = Rn $ \a -> (runRn f a) (runRn x a)

instance Monad (Rn r) where
  return = pure
  x >>= f = Rn $ \a -> (runRn (f ((runRn x) a))) a

askRn :: Rn a a
-- askRn = Rn $ \a -> a
askRn = Rn id

tomRn :: Rn MyEnv String
tomRn = do
    env <- askRn
    return (env ++ " This is Tom.")

jerryRn :: Rn MyEnv String
jerryRn = do
  env <- askRn
  return (env ++ " This is Jerry.")

tomAndJerryRn :: Rn MyEnv String
tomAndJerryRn = do
    t <- tomRn
    j <- jerryRn
    return (t ++ "\n" ++ j)

runJerryRunRn :: String
runJerryRunRn = (runRn tomAndJerryRn) "Who is this?"

-- Using Reader monad (type Reader r = ReaderT r Identity)
tomRM :: Reader MyEnv String
tomRM = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerryRM :: Reader MyEnv String
jerryRM = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerryRM :: Reader MyEnv String
tomAndJerryRM = do
    t <- tomRM
    j <- jerryRM
    return (t ++ "\n" ++ j)

runJerryRunRM :: String
runJerryRunRM = (runReader tomAndJerryRM) "Who is this?"

-- Using MonadReader
tomMR :: (MonadReader MyEnv m) => m String
tomMR = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerryMR :: (MonadReader MyEnv m) => m String
jerryMR = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerryMR :: (MonadReader MyEnv m) => m String
tomAndJerryMR = do
    t <- tomMR
    j <- jerryMR
    return (t ++ "\n" ++ j)

runJerryRunMR :: String
runJerryRunMR = (runReader tomAndJerryMR) "Who is this?"

main :: IO ()
main = do
  putStrLn "threading trough the functions"
  let out = runJerryRun
  putStrLn out

  putStrLn "using custom reader"
  let outR = runJerryRunR
  putStrLn outR

  putStrLn "using custom reader with newtype"
  let outRn = runJerryRunRn
  putStrLn outRn

  putStrLn "using Reader Monad"
  let outRM = runJerryRunRM
  putStrLn outRM

  putStrLn "using MonadReader"
  let outMR = runJerryRunMR
  putStrLn outMR
