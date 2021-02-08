{-# LANGUAGE Rank2Types #-}

module Task7
  ( cd
  , ls
  , file
  ) where

import Lens.Micro (Traversal', (^.), each, filtered)
import Task6 (FS (..), contents, name, _Dir, _File)

-- | Change directory to subdirectory with given name
cd
  :: FilePath  -- ^ Subdirectory name
  -> Traversal' FS FS  -- ^ Traversal with chanhing current directory
cd toDir =  _Dir . contents . each . filtered (\fs -> fs ^. name == toDir)

-- | Returns content of current directory
ls :: Traversal' FS FilePath
ls = _Dir . contents . each . name

-- | Returns file name if it exists in directory
file
  :: FilePath  -- ^ File name
  -> Traversal' FS String  -- ^ Traversal for showing file name
file fileName = _Dir . contents . each . _File . name . filtered (== fileName)
