module FSException
    ( FileSystemException (..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | Data type for exceptions in file system
data FileSystemException
  = InvalidPathException String  -- ^ Path does not exist
  | InvalidPermissionsException String  -- ^ Not enough persmissions
  | NullDirectoryException String  -- ^ Directory does not exist
  | NullRootException String  -- ^ Directory does not have parent
  | NullFileException String  -- ^ File does not exist
  | DublicateDirectoryException String  -- ^ Directory name already exist
  | DublicateFileException String  -- ^ File name already exist

instance Exception FileSystemException

instance Show FileSystemException where
  show (InvalidPathException path) =
    "InvalidPathException: " ++ "path '" ++ path ++ "' doesn't exist"
  show (InvalidPermissionsException file) =
     "InvalidPermissionsException: " ++  "not enough permissions for '" ++ file ++ "'"
  show (NullDirectoryException dir) =
     "NullDirectoryException: " ++ "directory with name '" ++ dir ++ "' doesn't exist"
  show (NullRootException dir) =
     "NullRootException: " ++ "you are already in root '" ++ dir ++ "' which you specified"
  show (DublicateDirectoryException dir) =
     "DublicateDirectoryException: " ++ "directory with name '" ++ dir ++ "' already exist"
  show (NullFileException file) =
     "NullFileException: " ++ "file with name '" ++ file ++ "' doesn't exist"
  show (DublicateFileException file) =
     "DublicateFileException: " ++ "file with name '" ++ file ++ "' already exist"
