{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll      #-}

module Task5
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | returns list splitted by separator
splitOn
  :: forall a . Eq a  -- ^ to compare element with separator
  => a  -- ^ separator
  -> [a]  -- ^ given list
  -> NonEmpty [a]  -- ^ 'NonEmpty' list of elements
splitOn separator list = splittedNonEmptyList
  where
    checkElements :: Eq a => a -> NonEmpty [a] -> NonEmpty [a]
    checkElements element (x :| xs) =
      if element == separator
      then [] :| x : xs
      else (element : x) :| xs
    splittedNonEmptyList = foldr checkElements ([] :| []) list

-- | returns list with separator between elements of given list
joinWith
  :: Eq a  -- ^ to compare element with separator
  => a  -- ^ separator
  -> NonEmpty [a]  -- ^ 'NonEmpty' list to merge
  -> [a]  -- ^ list merged with help of separator
joinWith sep (frst :| list) = frst ++ (foldr (\x xs -> sep : (x ++ xs)) [] list)
