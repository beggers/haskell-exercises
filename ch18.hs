module CH18 where

import Control.Monad (join)


data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure b = Second b
    (<*>) (First a) _ = First a
    (<*>) _ (First a) = First a
    (<*>) (Second f) (Second b) = Second $ f b

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a 


j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = f <$> m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = (f <$> m1) <*> m2

a :: Monad m => m a -> m (a -> b) -> m b
a m m' = m' <*> m

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = fmap (:) (f x) <*> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id
