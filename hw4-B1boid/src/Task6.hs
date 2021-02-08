module Task6
  ( FS(..)
  , getDirectory'
  , name
  , contents
  , _Dir
  , _File
  ) where

import System.FilePath (takeFileName, splitPath, (</>))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Lens.Micro (Lens', Traversal', lens)

-- | Data type of tree-like files system
data FS
    = Dir
    { _name     :: FilePath  -- ^ Directory name
    , _contents :: [FS]  -- ^ Subdirectories
    }
    | File
    { _name     :: FilePath  -- ^ File name
    }
    deriving (Show, Eq)

-- Build a tree-like files system by given filepath
getDirectory'
  :: FilePath  -- ^ Given file path of a root
  -> IO FS  -- ^ Tree-like files system
getDirectory' path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  if isDirectory
   then do
     directories <- listDirectory path
     content <- mapM (\fs -> getDirectory' (path </> fs)) directories
     return $ Dir (last $ splitPath path) content
   else if isFile
     then return $ File (takeFileName path)
     else return $ error $ "Wrong path:" <> path

-- | Lens for name field of 'FS'
name :: Lens' FS FilePath
name = lens _name (\fs name' -> fs { _name = name' })

-- | Lens for contents field of 'FS'
contents :: Traversal' FS [FS]
contents = _Dir . lens _contents (\fs contents' -> fs { _contents = contents' })

-- | Prism for Dir constructor of FS
_Dir :: Traversal' FS FS
_Dir f fs@(Dir _ _) = f fs
_Dir _ fs           = pure fs

-- | Prism for Dir constructor of FS
_File :: Traversal' FS FS
_File f fs@(File _) = f fs
_File _ fs          = pure fs
