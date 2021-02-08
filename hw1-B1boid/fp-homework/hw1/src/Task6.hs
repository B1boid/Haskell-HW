module Task6
  ( maybeConcat
  , eitherConcat
  ) where

-- | returns concatenation of unwrapped 'Maybe' elements of given list
maybeConcat
  :: [Maybe [a]]  -- ^ list of lists of 'Maybe' list
  -> [a]  -- ^ concatenation of unwrapped elements
maybeConcat []                     = []
maybeConcat ((Just list) : others) = list ++ maybeConcat others
maybeConcat (Nothing     : others) = maybeConcat others

-- | returns concatenation of unwrapped 'Either' separately
eitherConcat
  :: (Monoid a, Monoid b)  -- ^ elements 'Monoid's
  => [Either a b]  -- ^ list of 'Either' elements
  -> (a, b)  -- ^ pair of concatenation of separately unwrapped elements
eitherConcat = foldr separateConcat (mempty, mempty)
  where
    separateConcat :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
    separateConcat (Left l)  (ls, rs) = (l <> ls, rs)
    separateConcat (Right r) (ls, rs) = (ls, r <> rs)
