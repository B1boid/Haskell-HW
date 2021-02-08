{-# LANGUAGE InstanceSigs #-}

module Block1
  ( Tree(..)
  , NonEmpty(..)

  , stringSum
  ) where

import Text.Read (readMaybe)
import Control.Applicative (liftA2)

-- | returns sum of numbers in string if there are only numbers
-- otherwise returns Nothing
stringSum
  :: String  -- ^ given string
  -> Maybe Int  -- ^ sum of numbers
stringSum = fmap sum . traverse readMaybe . words

-- | simple tree
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f       <*> tree = fmap f tree
  Branch lf rf <*> tree = Branch (lf <*> tree) (rf <*> tree)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f tree (Leaf a)     = f a tree
  foldr f tree (Branch l r) = foldr f (foldr f tree r) l

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Branch l r) = liftA2 Branch (traverse f l) (traverse f r)

-- | non-empty list
data NonEmpty a = a :| [a]

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = f x :| (fmap f xs ++ (fs <*> x : xs))

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = liftA2 (:|) (f x) (traverse f xs)

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  x :| xs >>= f = y :| ys
    where
      toList (a :| as) = a : as
      y : ys = x : xs >>= toList . f
