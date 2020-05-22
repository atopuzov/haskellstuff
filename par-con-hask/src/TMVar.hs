module TMVar where

import Control.Concurrent.STM (TVar, STM, newTVar, retry, readTVar, writeTVar, orElse)


newtype TMVar a = TMVar (TVar (Maybe a))


newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return $ TMVar t

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a -> do
      writeTVar t Nothing
      return a

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) val = do
  m <- readTVar t
  case m of
    Nothing -> do
      writeTVar t (Just val)
      return ()
    Just _ -> retry

takeEitherTMVar :: TMVar a -> TMVar b -> STM (Either a b)
takeEitherTMVar ma mb = do
  fmap Left (takeTMVar ma) `orElse` fmap Right (takeTMVar mb)


main = undefined
