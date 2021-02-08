module FSInit
  ( readFileSystem
  ) where

import qualified Data.ByteString as B
import Data.List.Split (splitOn)
import Control.Exception (throw)
import FSException (FileSystemException (..))
import FSTypes (Directories (..), Directory (..), DirInfo (..), File (..), FileContent (..),
                FileInfo(..))
import System.Directory (doesDirectoryExist, getCurrentDirectory, getModificationTime,
                         getPermissions, getFileSize, listDirectory, readable)
import System.FilePath (FilePath, takeExtensions)


-- | Read and remember file system from current root path
readFileSystem
  :: FilePath  -- ^ Root path
  -> IO Directories  -- ^ Directories in a start state
readFileSystem fPath = do
  tmpDir <- getCurrentDirectory
  let curDir = if fPath == "" then tmpDir else (if last fPath == '/' then init fPath else fPath)
  directories <- createAllDirectories (curDir : [])
  return $ Directories directories curDir

-- | Create all directories and files in them
createAllDirectories
  :: [FilePath]  -- ^ Directory paths to check
  -> IO [Directory]  -- ^ All directories with files from the current path
createAllDirectories [] = return []
createAllDirectories (dirPath : others) = do
  filesPathsInCurDir <- getFilesPath dirPath
  filesInCurDir <- getFilesFromList filesPathsInCurDir
  filesPath <- getFilesPath dirPath
  perm <- getPermissions dirPath
  relativePaths <- listDirectory dirPath
  subDirsPath <- getPathsFromList False (getAbsolutePaths dirPath relativePaths) []
  subDirs <- createAllDirectories subDirsPath
  othersDirs <- createAllDirectories others
  let dirInfo = DirInfo dirPath (length filesPath) 0 perm
  return $ ((Directory filesInCurDir dirInfo) : subDirs) ++ othersDirs

-- | Get all files paths in the given directory path
-- * 'InvalidPathException'
--    Directory with this path does not exist
-- * 'InvalidPermissionsException'
--    User do not have enough permissions to read
getFilesPath
  :: FilePath  -- ^ Directory path
  -> IO [FilePath]  -- ^ All files paths in given directory
getFilesPath fPath = do
  isDir <- doesDirectoryExist fPath
  if not isDir
  then
    throw $ InvalidPathException fPath
  else do
    permissions <- getPermissions fPath
    if not (readable permissions)
    then
      throw $ InvalidPermissionsException fPath
    else do
      relativePaths <- listDirectory fPath
      let absolutePaths = getAbsolutePaths fPath relativePaths
      files <- getPathsFromList True absolutePaths []
      return files

-- | Get absolute paths by relative
getAbsolutePaths
  :: FilePath  -- ^ Path to add to relatives paths
  -> [FilePath]  -- ^ Relative paths
  -> [FilePath]  -- ^ Full absolute paths
getAbsolutePaths _   []       = []
getAbsolutePaths add (x : xs) = (add ++ "/" ++ x) : (getAbsolutePaths add xs)

-- | Get all paths of files or directories
getPathsFromList
  :: Bool  -- ^ True if files paths needed
  -> [FilePath]  -- ^ Given paths to check
  -> [FilePath]  -- ^ Accumulated paths
  -> IO [FilePath]  -- ^ Return all files paths or directory paths
getPathsFromList _       []       filePaths = return filePaths
getPathsFromList isFiles (x : xs) acc = do
  isDir <- doesDirectoryExist x
  if (not isFiles && isDir) || (isFiles && not isDir)
  then
    getPathsFromList isFiles xs (x : acc)
  else
    getPathsFromList isFiles xs acc

-- | Get all files by given files paths
-- * 'InvalidPermissionsException'
--    User do not have enough permissions to read
getFilesFromList
  :: [FilePath]  -- ^ Files paths
  -> IO [File]  -- ^ All files by given files paths
getFilesFromList [] = return []
getFilesFromList (fp : fps) = do
  permissions <- getPermissions fp
  if readable permissions
  then do
    curData <- B.readFile fp
    let curContent = FileContent curData
    let type' = takeExtensions fp
    time <- getModificationTime fp
    size <- getFileSize fp
    let fileInfo = FileInfo fp permissions type' time size
    xsFiles <- getFilesFromList fps
    return $ [File curContent fileInfo] ++ xsFiles
  else
    throw $ InvalidPermissionsException fp
