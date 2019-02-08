import Control.Applicative ((<*>))

-- https://medium.com/@fintan.halpenny/compose-tetris-196b70035aff

newtype Compose f g a = Compose { unCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> Compose f g a -> Compose f g b
  -- fmap f (Compose fg) = Compose $ fmap togb fg
  --   where
  --     -- gb :: g a -> gb
  --     togb ga = fmap f ga

  -- fmap f (Compose fg) = Compose $ fmap (fmap f) fg

  -- fmap f fg = Compose $ fmap (fmap f) (unCompose fg)

  fmap f = Compose . fmap (fmap f) . unCompose


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- pure :: a -> f a
  -- pure :: a -> Compose f g a
  pure = Compose . pure . pure
  -- f (a -> b) -> f a -> f b
  -- Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose k) = Compose $ (<*>) <$> f <*> k
