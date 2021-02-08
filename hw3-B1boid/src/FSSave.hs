module FSSave
  ( saveChanges
  ) where

import System.Directory (doesDirectoryExist, createDirectory, removeDirectory, removeFile)
import System.IO (writeFile)
import Data.List (elemIndices)
import Safe (lastMay)
import Data.Maybe (fromMaybe)
import FSTypes(Directories (..), Directory (..), DirInfo (..), File (..), FileContent (..),
               FileInfo(..))
import FileSystem (findDirWithPath, getSubDirectories, getFileWithPath)
import qualified Data.ByteString as B

-- | Write new file system to real file system
saveChanges
  :: Directories  -- ^ Old directories that were readed on start of a program
  -> Directories  -- ^ New directories after changes
  -> IO ()  -- ^ Action with real world without chages
saveChanges oldDirs newDirs = do
  removeFromReal (dirs oldDirs) (dirs oldDirs) (dirs newDirs)
  addToReal (dirs newDirs) (dirs oldDirs)

-- | Remove from real file system all files and directories that are not in new file system
removeFromReal
  :: [Directory]  -- ^ Directoires for proccessing
  -> [Directory]  -- ^ Old directories
  -> [Directory]  -- ^ New directories
  -> IO ()  -- ^ Action with real world without chages
removeFromReal []           _       _       = return ()
removeFromReal (dir : dirs) oldDirs newDirs = do
  let curDirPath = dirPath $ dirInfo dir
  case findDirWithPath curDirPath newDirs of
    Just dirInNew -> removeFiles (curFiles dir) (curFiles dirInNew)
    Nothing       -> removeSub (dir : []) oldDirs
  removeFromReal dirs oldDirs newDirs

-- | Remove all sub directories from real file system if parent were deleted
removeSub
  :: [Directory]  -- ^ Directoires for proccessing
  -> [Directory]  -- ^ Old directories
  -> IO ()  -- ^ Action with real world without chages
removeSub []           _       = return ()
removeSub (dir : dirs) oldDirs = do
  let curDirPath = dirPath $ dirInfo dir
  isExist <- doesDirectoryExist curDirPath
  if isExist
  then do
    sequenceA $ fmap (removeFile . filePath . fileInfo) (curFiles dir)
    removeSub (getSubDirectories curDirPath oldDirs []) oldDirs
    removeDirectory curDirPath
    removeSub dirs oldDirs
  else
    removeSub dirs oldDirs

-- | Remove files from real file system that were deleted
removeFiles
  :: [File]  -- ^ Old files for proccessing
  -> [File]  -- ^ New files
  -> IO ()  -- ^ Action with real world without chages
removeFiles []             _        = return ()
removeFiles (file : files) newFiles = do
  let curPath = filePath $ fileInfo file
  case getFileWithPath curPath newFiles of
    Nothing -> removeFile curPath
    Just _  -> return ()
  removeFiles files newFiles

-- | Add to real file system all files and directories that appear in new file system
addToReal
  :: [Directory]  -- ^ New directorires for proccessing
  -> [Directory]  -- ^ Old directorires
  -> IO ()  -- ^ Action with real world without chages
addToReal []           _       = return ()
addToReal (dir : dirs) oldDirs = do
  let curDirPath = dirPath $ dirInfo dir
  case findDirWithPath curDirPath oldDirs of
    Just dirInOld -> addFiles (curFiles dir) (curFiles dirInOld)
    Nothing -> do
      addDirs curDirPath
      addFiles (curFiles dir) []
  addToReal dirs oldDirs

-- | Create new directory in real file system
addDirs
  :: FilePath  -- ^ New directory path
  -> IO ()  -- ^ Action with real world without chages
addDirs path = do
  isExist <- doesDirectoryExist path
  if isExist
  then
    return ()
  else do
    addDirs $ take (fromMaybe 0 $ lastMay $ elemIndices '/' path) path
    createDirectory path

-- | Add new files to real file system
addFiles
  :: [File]  -- ^ New files for proccessing
  -> [File]  -- ^ Old files
  -> IO ()  -- ^ Action with real world without chages
addFiles []             _        = return ()
addFiles (file : files) oldFiles = do
  let curFilePath = (filePath $ fileInfo file)
  let (FileContent curFileData) = (fileContent file)
  case getFileWithPath curFilePath oldFiles of
    Nothing      -> do
      B.writeFile curFilePath curFileData
      addFiles files oldFiles
    Just oldFile -> do
      if (fileContent file) == (fileContent oldFile)
      then
        addFiles files oldFiles
      else do
        B.writeFile curFilePath curFileData
        addFiles files oldFiles
