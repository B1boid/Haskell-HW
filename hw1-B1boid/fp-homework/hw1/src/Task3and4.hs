{-# LANGUAGE InstanceSigs #-}

module Task3and4
  ( Tree(..)

  , isEmpty
  , treeSize
  , find
  , insert
  , fromList
  , remove
  ) where

import Data.List.NonEmpty (NonEmpty (..))

data Tree a
  = Leaf
  | Node (Tree a) (NonEmpty a) (Tree a)
  deriving (Show)

-- | checks if the 'Tree' is empty
isEmpty
  :: Tree a  -- ^ 'Tree' to check
  -> Bool  -- ^ True if it is 'Leaf'
isEmpty Leaf = True
isEmpty _    = False

-- | returns size of the 'Tree'
treeSize
  :: Tree a  -- ^ 'Tree' to check
  -> Int  -- ^ size of 'Tree'
treeSize Leaf            = 0
treeSize (Node l list r) = length list + treeSize l + treeSize r

-- | checks if the element is in the 'Tree'
find
  :: Ord a  -- ^ 'Tree' condition
  => Tree a  -- ^ 'Tree' to check in
  -> a  -- ^ element to find
  -> Bool  -- ^ True if the element is succesfully found
find Leaf            _       = False
find (Node l (first :| _) r) element =
  case compare element first of
    EQ -> True
    LT -> find l element
    GT -> find r element

-- | returns the 'Tree' with given element
insert
  :: Ord a  -- ^ 'Tree' condition
  => Tree a  -- ^ 'Tree' to insert in
  -> a  -- ^ element to insert
  -> Tree a  -- ^ new 'Tree' with given element
insert Leaf            element = Node Leaf (element :| []) Leaf
insert (Node l (first :| others) r) element =
  case compare element first of
    EQ -> Node l (element :| (first : others)) r
    LT -> Node (insert l element) (first :| others) r
    GT -> Node l (first :| others) (insert r element)

-- | returns 'Tree' from given list.
fromList
  :: Ord a  -- ^ 'Tree' condition
  => NonEmpty a  -- ^ given list
  -> Tree a  -- ^ 'Tree' from list
fromList = foldl insert Leaf

-- | returns the 'Tree' without given element
remove
  :: Ord a  -- ^ 'Tree' condition
  => Tree a  -- ^ 'Tree' to remove in
  -> a  -- ^ element to remove
  -> Tree a  -- ^ new 'Tree' without given element
remove Leaf _ = Leaf
remove (Node l (first :| others) r) element =
  case compare element first of
    LT -> Node (remove l element) (first :| others) r
    GT -> Node l (first :| others) (remove r element)
    EQ -> change l r where
      change :: (Ord a) => Tree a -> Tree a -> Tree a
      change Leaf right = right
      change left Leaf  = left
      change left right = Node left (minE :| minRList) (remove right minE) where
        (minE :| minRList) = findMin right
        findMin :: (Ord a) => Tree a -> NonEmpty a
        findMin Leaf                        = error "tree is empty"
        findMin (Node Leaf    resMinList _) = resMinList
        findMin (Node minTree _          _) = findMin minTree


instance Foldable Tree where
  -- | foldMap for the 'Tree'
  foldMap
    :: Monoid m  -- ^ Monoid to combine elements of the same type
    => (a -> m)  -- ^ element to monoid mapping function
    -> Tree a  -- ^ 'Tree' to apply foldMap on
    -> m  -- ^ resulting monoid
  foldMap _ Leaf            = mempty
  foldMap f (Node l list r) = foldMap f l <> foldMap f list <> foldMap f r

  -- | foldr for the 'Tree'
  foldr
    :: (a -> b -> b)  -- ^ applying function
    -> b  -- ^ value of 'Tree'
    -> Tree a  -- ^ given 'Tree' to fold
    -> b  -- ^ result of folding
  foldr _ z Leaf            = z
  foldr f z (Node l list r) = foldr f (foldr f (foldr f z r) list) l
