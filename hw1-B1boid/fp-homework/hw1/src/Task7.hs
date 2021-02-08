module Task7
  ( NonEmpty(..)
  , ThisOrThat(..)
  , Name(..)
  , Endo(..)
  ) where

-- | List with minimum one element
data NonEmpty a
  = a :| [a]
  deriving(Eq, Show)

instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))

-- | Data with two conditions or both together
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving(Eq, Show)

instance Semigroup (ThisOrThat a b) where
  This a         <> That b   = Both a b
  That b         <> This a   = Both a b
  This a         <> Both _ b = Both a b
  That b         <> Both a _ = Both a b
  res@(This _)   <> This _   = res
  res@(That _)   <> That _   = res
  res@(Both _ _) <> _        = res

-- | Data with name or empty
data Name
  = Name String
  | EmptyName
  deriving (Show, Eq)

instance Semigroup Name where
  Name a     <> Name b     = Name (a ++ ('.' : b))
  EmptyName  <> a          = a
  a          <> EmptyName  = a

instance Monoid Name where
  mempty = EmptyName

-- | Data with the function of one argument that returns the same type
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo x <> Endo y = Endo (x . y)

instance Monoid (Endo a) where
  mempty = Endo id
