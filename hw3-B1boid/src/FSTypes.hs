module FSTypes
  ( DirDataNames (..)
  , Directories (..)
  , Directory (..)
  , DirInfo (..)
  , FileInfo (..)
  , File (..)
  , FileContent (..)
  , FilesManager (..)
  , FilePaths (..)
  ) where

import System.IO (FilePath)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.Time.Clock (UTCTime (..))
import System.Directory(Permissions (..))
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString as B

-- | Type for file system
type FilesManager e a = ExceptT e (State Directories) a

-- | Data type for directories
data Directories = Directories
  { dirs        :: [Directory]  -- ^ All directories
  , curDirsPath :: FilePath  -- ^ Current absolute path
  }
  deriving (Show, Eq)

-- | Data type for directory
data Directory = Directory
  { curFiles :: [File]  -- ^ All files in directory
  , dirInfo  :: DirInfo  -- ^ Information about this directory
  }
  deriving (Show, Eq)

-- | Data type for directory information
data DirInfo = DirInfo
  { dirPath     :: FilePath  -- ^ Directory absolute path
  , filesNumber :: Int  -- ^ Files number in the directory
  , dirSize     :: Integer  -- ^ Directory size
  , dirPerm     :: Permissions  -- ^ Permissions of the directory
  }
  deriving Eq

-- | Data type for file
data File = File
  { fileContent :: FileContent  -- ^ File data
  , fileInfo    :: FileInfo  -- ^ Information about this file
  }
  deriving (Show, Eq)

-- | Data type for file information
data FileInfo = FileInfo
  { filePath :: FilePath  -- ^ File absolute path
  , filePerm :: Permissions  -- ^ File permissions
  , fileType :: String  -- ^ File extension
  , fileTime :: UTCTime  -- ^ File last modification time
  , fileSize :: Integer  -- ^ File size
  }
  deriving Eq

-- | Data type for representation content of directory
data DirDataNames = DirDataNames [String] [String]

-- | Files pathes
newtype FilePaths = FilePaths [FilePath]

-- | File data
newtype FileContent = FileContent B.ByteString
  deriving Eq

instance Show FileContent where
  show (FileContent content) = toString content

instance Show FilePaths where
  show (FilePaths [])             = ""
  show (FilePaths (path : paths)) = "-> " ++ path ++ "\n" ++ show (FilePaths paths)

instance Show FileInfo where
  show (FileInfo filePath filePerm fileType fileTime fileSize) =
    "path: " ++ filePath ++
    "\npermissions: " ++ show(filePerm) ++
    "\ntype: " ++ fileType ++
    "\ntime: " ++ show(fileTime) ++
    "\nsize: " ++ show(fileSize) ++ " B"

instance Show DirInfo where
  show (DirInfo dirPath filesNumber dirSize dirPerm) =
    "path: " ++ dirPath ++
    "\nfiles number: " ++ show(filesNumber) ++
    "\nsize: " ++ show(dirSize) ++ " B" ++
    "\npermissions: " ++ show(dirPerm)

instance Show DirDataNames where
  show (DirDataNames files (dir : dirs)) = "|--" ++ dir  ++ "\n" ++ show (DirDataNames files dirs)
  show (DirDataNames (file : files) [])  = "|-"  ++ file ++ "\n" ++ show (DirDataNames files [])
  show (DirDataNames [] []) = ""
