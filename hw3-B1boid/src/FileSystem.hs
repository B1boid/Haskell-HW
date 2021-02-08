module FileSystem
  ( applyCommand
  , findDirWithPath
  , getSubDirectories
  , getFileWithPath
  , defPerms
  , defTime
  , getMessage
  , dir
  , ls
  , cat
  , find
  , info
  , infodir
  , showColorText
  ) where

import Data.List.Split (splitOn)
import Data.List (isPrefixOf, elemIndices, intercalate)
import FSException (FileSystemException(..))
import FSTypes (FilesManager, Directories(..), Directory(..), DirInfo(..), File(..),
                FileContent(..), FileInfo(..), FilePaths(..), DirDataNames(..))
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad.State (put, modify, get, runState)
import Parser (Command(..), helpText)
import Safe (lastMay)
import Data.Maybe (fromMaybe)
import System.Directory (Permissions (..), setOwnerReadable, setOwnerSearchable,
                        setOwnerWritable, emptyPermissions)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar (Day(..))
import System.FilePath (FilePath, takeExtensions)
import Data.ByteString.UTF8 (fromString, toString)
import Rainbow (Chunk (..), Radiant (..), chunk, fore, green, red, putChunkLn)
import Data.Text (pack)
import qualified Data.ByteString as B
import qualified System.Process as SP


-- | Apply command to current directories and returns new
applyCommand
  :: Command  -- ^ Command given by user
  -> Directories  -- ^ Current directories
  -> IO (Directories)  -- ^ Directories after command applying
applyCommand (CD dirName) directories =
  applyWithChanges "Successfully changed current directory" (cd dirName) directories
applyCommand (MKDIR dirName) directories =
  applyWithChanges "Folder created successfully" (mkdir dirName) directories
applyCommand (TOUCH fileName) directories =
  applyWithChanges "Successfully changed current directory" (touch fileName) directories
applyCommand (RMDIR dirName) directories =
  applyWithChanges "Folder deleted successfully" (rmdir dirName) directories
applyCommand (RM fileName) directories =
  applyWithChanges "File deleted successfully" (rm fileName) directories
applyCommand (EDIT fileName text) directories =
  applyWithChanges "File edited successfully" (edit fileName text) directories
applyCommand (COPY fileNameToCopy newFileName) directories =
  applyWithChanges "File copy created successfully" (copy fileNameToCopy newFileName) directories
applyCommand DIR dirs' = applyWithShow dir dirs'
applyCommand (LS dirName) dirs' = applyWithShow (ls dirName) dirs'
applyCommand (CAT fileName) dirs' = applyWithShow (cat fileName) dirs'
applyCommand (FIND fileName) dirs' = applyWithShow (find fileName) dirs'
applyCommand (INFO fileName) dirs' = applyWithShow (info fileName) dirs'
applyCommand (INFODIR dirName) dirs' = applyWithShow (infodir dirName) dirs'
applyCommand HELP directories = do
  putStrLn helpText
  return directories
applyCommand CLR directories = do
  _ <- SP.system "reset"
  return directories

-- | Apply command to current directories, shows error or success message and returns new
applyWithChanges
  :: String  -- ^ Message of the result
  -> FilesManager FileSystemException ()  -- ^ File system update
  -> Directories  -- ^ Current directories
  -> IO (Directories)  -- ^ Directories after command applying
applyWithChanges text func directories = do
  let (res, newDirs) = runState (runExceptT $ func) directories
  case res of
    Left e  -> do
      putStrLn $ show e
      return directories
    Right _ -> do
      showColorText green text
      return newDirs

-- | Show message of given command's function
applyWithShow
  :: FilesManager FileSystemException String  -- ^ File system show info
  -> Directories  -- ^ Current directories
  -> IO (Directories)  -- ^ Directories after command applying
applyWithShow func directories = do
  putStrLn $ getMessage func directories
  return directories

-- | Show color text
showColorText
  :: Radiant  -- ^ Message color
  -> String  -- ^ Current message
  -> IO ()  -- ^ Action with real world without chages
showColorText curColor curStr = putChunkLn $ fore curColor $ chunk $ pack curStr

-- | Get success message or error
getMessage
  :: FilesManager FileSystemException String  -- ^ File system show info
  -> Directories  -- ^ Current directories
  -> String  -- ^ Message or error
getMessage func directories = do
  let (res, _) = runState (runExceptT $ func) directories
  case res of
    Left e  -> show e
    Right x -> x

-- | Function for CD command
cd
  :: String  -- ^ Given directory name
  -> FilesManager FileSystemException ()  -- ^ File system update
cd ".." = do
  directories <- get
  let curPath = curDirsPath directories
  case getParentDir curPath (dirs directories) of
    Nothing  -> throwError $ NullRootException curPath
    Just dir -> modify (changeCurDirPath (dirPath $ dirInfo dir))
cd name = do
  directories <- get
  case findDirWithPath (getPathByName name directories) (dirs directories) of
    Nothing  -> throwError $ NullDirectoryException name
    Just dir -> modify (changeCurDirPath (dirPath $ dirInfo dir))

-- | Find directory with given path
findDirWithPath
  :: FilePath  -- ^ Given path
  -> [Directory]  -- ^ Current directories
  -> Maybe Directory  -- ^ Finded directory
findDirWithPath _    []             = Nothing
findDirWithPath path (dir : dirs)
  | path == (dirPath $ dirInfo dir) = Just dir
  | otherwise                       = findDirWithPath path dirs

-- | Find directory with given path
getParentDir
  :: FilePath  -- ^ Given path
  -> [Directory]  -- ^ Current directories
  -> Maybe Directory
getParentDir _    [] = Nothing
getParentDir path (dir : dirs)
  | (dirPath $ dirInfo dir) == take (fromMaybe 0 $ lastMay $ elemIndices '/' path) path = Just dir
  | otherwise = getParentDir path dirs

-- | Get path by name
getPathByName
  :: String  -- ^ Given name
  -> Directories  -- ^ Current directories
  -> FilePath  -- ^ Returns path
getPathByName name directories = (curDirsPath directories) ++ "/" ++ name

-- | Change current path
changeCurDirPath
  :: FilePath  -- ^ Given new path
  -> Directories  -- ^ Current directories
  -> Directories  -- ^ New directories
changeCurDirPath newPath (Directories a _) = Directories a newPath

-- | Function for DIR command
dir :: FilesManager FileSystemException String
dir = do
  directories <- get
  getDirDesc $ curDirsPath directories

-- | Function for LS command
ls
  :: String  -- ^ Directory name
  -> FilesManager FileSystemException String  -- ^ File system's shown information
ls dirName = do
  directories <- get
  getDirDesc $ getPathByName dirName directories

-- | Get directory content by given path
getDirDesc
  :: String  -- ^ Directory path
  -> FilesManager FileSystemException String  -- ^ File system's shown information
getDirDesc path = do
  directories <- get
  let cDirs = dirs directories
  case findDirWithPath path cDirs of
    Nothing  -> throwError $ NullDirectoryException $ getNameFromPath path
    Just dir -> do
      let filesNames = map (getNameFromPath . filePath . fileInfo) (curFiles dir)
      let dirsNames  = map (getNameFromPath . dirPath  . dirInfo)  (getSubDirectories path cDirs [])
      return $ show $ DirDataNames filesNames dirsNames

-- | Get name form path
getNameFromPath
  :: FilePath  -- ^ Given path
  -> String  -- ^ Returns name
getNameFromPath path = last (splitOn "/" path)

-- | Get sub directorires
getSubDirectories
  :: FilePath  -- ^ Given path
  -> [Directory]  -- ^ Proccessing directories
  -> [Directory]  -- ^ Accumulated sub directories
  -> [Directory]  -- ^ Returns sub directories
getSubDirectories _    []           subDirs = subDirs
getSubDirectories path (dir : dirs) subDirs
  | isPrefixOf path curDirPath && length (splitOn "/" path) + 1 == length (splitOn "/" curDirPath)
    = getSubDirectories path dirs (dir : subDirs)
  | otherwise
    = getSubDirectories path dirs subDirs
    where
      curDirPath = dirPath $ dirInfo dir

-- | Function for MKDIR command
mkdir
  :: String  -- ^ Directory name
  -> FilesManager FileSystemException ()  -- ^ File system update
mkdir name = do
  directories <- get
  let pathForNewDir = getPathByName name directories
  case findDirWithPath pathForNewDir (dirs directories) of
    Just _  -> throwError $ DublicateDirectoryException name
    Nothing -> do
      let newDirInfo = DirInfo
                         { dirPath     = pathForNewDir
                         , filesNumber = 0
                         , dirSize     = 0
                         , dirPerm     = defPerms
                         }
      let newDir = Directory [] newDirInfo
      modify $ updateDirs (newDir : (dirs directories))

-- | Function for CAT command
cat
  :: String  -- ^ File name
  -> FilesManager FileSystemException String  -- ^ File system's shown information
cat name = do
  directories <- get
  let (Just dir) = findDirWithPath (curDirsPath directories) (dirs directories)
  case getFileWithPath (getPathByName name directories) (curFiles dir) of
    Nothing   -> throwError $ NullFileException name
    Just file -> return $ show $ fileContent file

-- | Get file with path
getFileWithPath
  :: FilePath  -- ^ Given path
  -> [File]  -- ^ Current files
  -> Maybe File  -- ^ Returns file if exists
getFileWithPath _    []                = Nothing
getFileWithPath path (file : files)
  | path == (filePath $ fileInfo file) = Just file
  | otherwise                          = getFileWithPath path files

-- | Function for TOUCH command
touch
  :: String  -- ^ File name
  -> FilesManager FileSystemException ()  -- ^ File system update
touch name = do
  createFileWithContent name ""

-- | Create file with given content
createFileWithContent
  :: String  -- ^ File name
  -> String  -- ^ File content
  -> FilesManager FileSystemException ()  -- ^ File system update
createFileWithContent name curFileContent = do
  directories <- get
  let pathForNewFile = getPathByName name directories
  let (Just dir) = findDirWithPath (curDirsPath directories) (dirs directories)
  case getFileWithPath pathForNewFile (curFiles dir) of
    Just _  -> throwError $ DublicateFileException name
    Nothing -> do
      let newFileInfo = FileInfo
                          { filePath = pathForNewFile
                          , filePerm = defPerms
                          , fileType = (takeExtensions pathForNewFile)
                          , fileTime = defTime
                          , fileSize = toInteger $ 8 * length curFileContent
                          }
      let newFile = File (FileContent (fromString curFileContent)) newFileInfo
      let newDir = Directory (newFile : (curFiles dir)) (dirInfo dir)
      let (newDirs, _) = (remDirs False (curDirsPath directories) (dirs directories) [] False)
      modify $ updateDirs (newDir : newDirs)


-- | Update directories
updateDirs
  :: [Directory]  -- ^ New directories
  -> Directories  -- ^ Current directories
  -> Directories  -- ^ Returns new directories
updateDirs newDirs (Directories _ path) = Directories newDirs path

-- | Default permission
defPerms :: Permissions
defPerms = setOwnerSearchable True $ setOwnerWritable True $ setOwnerReadable True emptyPermissions

-- | Default time for purity
defTime :: UTCTime
defTime =  UTCTime  (ModifiedJulianDay 0) (secondsToDiffTime 0)

-- | Function for RMDIR command
rmdir
  :: String  -- ^ Directory name
  -> FilesManager FileSystemException ()  -- ^ File system update
rmdir name = do
  directories <- get
  case (remDirs True (getPathByName name directories) (dirs directories) [] False) of
    (newDirs, True)  -> modify $ updateDirs newDirs
    (_      , False) -> throwError $ NullDirectoryException name

-- | Remove directories
remDirs
  :: Bool  -- ^ True if want to remove all sub directories
  -> FilePath  -- ^ Directory path
  -> [Directory]  -- ^ Proccessing directories
  -> [Directory]  -- ^ Accumulated directories
  -> Bool  -- ^ True if directory exist
  -> ([Directory], Bool)  -- ^
remDirs _    _          []           newDirs exist = (newDirs, exist)
remDirs dAll removePath (dir : dirs) acc     exist
  | dAll && isPrefixOf (removePath ++ "/") curPath = remDirs dAll removePath dirs acc         True
  | removePath == curPath                          = remDirs dAll removePath dirs acc         True
  | otherwise                                      = remDirs dAll removePath dirs (dir : acc) exist
  where
    curPath = dirPath $ dirInfo dir

-- | Function for RM command
rm
  :: String  -- ^ File name
  -> FilesManager FileSystemException ()  -- ^ File system update
rm name = do
  directories <- get
  let pathForFile = getPathByName name directories
  let (Just dir) = findDirWithPath (curDirsPath directories) (dirs directories)
  case getFileWithPath pathForFile (curFiles dir) of
    Nothing   -> throwError $ NullFileException name
    Just file -> do
      let updCurFiles = remFile pathForFile (curFiles dir) []
      let newDir = Directory{curFiles=updCurFiles, dirInfo=(dirInfo dir)}
      let (newDirs, _) = (remDirs False (curDirsPath directories) (dirs directories) [] False)
      modify $ updateDirs (newDir : newDirs)

-- | Remove file by give path
remFile
  :: FilePath  -- ^ Given file path
  -> [File]  -- ^ Proccessing files
  -> [File]  -- ^ Accumulated files
  -> [File]  -- ^ Returns new files
remFile _          []             updFiles   = updFiles
remFile removePath (file : files) acc
  | removePath == (filePath $ fileInfo file) = remFile removePath files acc
  | otherwise                                = remFile removePath files (file : acc)

-- | Function for EDIT command
edit
  :: String  -- ^ File name
  -> [String]  -- ^ New file content
  -> FilesManager FileSystemException ()  -- ^ File system update
edit name text = do
  directories <- get
  let newText = intercalate " " text
  let pathForFile = getPathByName name directories
  let (Just dir) = findDirWithPath (curDirsPath directories) (dirs directories)
  case getFileWithPath pathForFile (curFiles dir) of
    Nothing   -> throwError $ NullFileException name
    Just file -> do
      let updCurFiles = remFile pathForFile (curFiles dir) []
      let newFileInfo = FileInfo
                          { filePath = pathForFile
                          , filePerm = defPerms
                          , fileType = (takeExtensions pathForFile)
                          , fileTime = defTime
                          , fileSize = toInteger $ 8 * length newText
                          }
      let newFile = File (FileContent (fromString newText)) newFileInfo
      let newDir = Directory (newFile : updCurFiles) (dirInfo dir)
      let (newDirs, _) = (remDirs False (curDirsPath directories) (dirs directories) [] False)
      modify $ updateDirs (newDir : newDirs)

-- | Function for FIND command
find
  :: String  -- ^ File name
  -> FilesManager FileSystemException String  -- ^ File system's shown information
find name = do
  directories <- get
  let subDirs = getAllSubDirs (curDirsPath directories) (dirs directories) []
  case findFileInDirs name subDirs [] of
    Nothing        -> throwError $ NullFileException name
    Just filePaths -> return $ show $ filePaths

-- | Get all sub directories
getAllSubDirs
  :: FilePath  -- ^ Directory file path
  -> [Directory]  -- ^ Proccessing directories
  -> [Directory]  -- ^ Accumulated directories
  -> [Directory]  -- ^ Returns directories
getAllSubDirs _    []           subDirs              = subDirs
getAllSubDirs path (dir : dirs) acc
  | path == (dirPath $ dirInfo dir)                  = getAllSubDirs path dirs (dir : acc)
  | isPrefixOf (path ++ "/") (dirPath $ dirInfo dir) = getAllSubDirs path dirs (dir : acc)
  | otherwise                                        = getAllSubDirs path dirs acc

-- | Find file in directories
findFileInDirs
  :: String  -- ^ File name
  -> [Directory]  -- ^ Proccessing directories
  -> [FilePath]  -- ^ Accumulated file paths
  -> Maybe FilePaths  -- ^ Returns file paths if finded
findFileInDirs _    []           []        = Nothing
findFileInDirs _    []           filePaths = Just $ FilePaths filePaths
findFileInDirs name (dir : dirs) acc = do
  let curPath = (dirPath $ dirInfo dir) ++ "/" ++ name
  case getFileWithPath curPath (curFiles dir) of
    Nothing -> findFileInDirs name dirs acc
    Just _  -> findFileInDirs name dirs (curPath : acc)

-- | Function for INFO command
info
  :: String  -- ^ File name
  -> FilesManager FileSystemException String  -- ^ File system's shown information
info name = do
  directories <- get
  let (Just dir) = findDirWithPath (curDirsPath directories) (dirs directories)
  case getFileWithPath (getPathByName name directories) (curFiles dir) of
    Nothing   -> throwError $ NullFileException name
    Just file -> return $ show $ fileInfo file

-- | Function for INFODIR command
infodir
  :: String  -- ^ Directory name
  -> FilesManager FileSystemException String  -- ^ File system's shown information
infodir name = do
  directories <- get
  case findDirWithPath (getPathByName name directories) (dirs directories) of
    Nothing  -> throwError $ NullDirectoryException name
    Just dir -> do
      let curDirInfo = dirInfo dir
      return $ show DirInfo
                      { dirPath     = dirPath curDirInfo
                      , filesNumber = length $ curFiles dir
                      , dirSize     = sum $ fmap (fileSize . fileInfo) (curFiles dir)
                      , dirPerm     = dirPerm curDirInfo
                      }

-- | Function for COPY command
copy
  :: String  -- ^ File name to copy
  -> String  -- ^ New file name with same content
  -> FilesManager FileSystemException ()  -- ^ File system update
copy fileNameToCopy newFileName = do
  directories <- get
  let (Just dir) = findDirWithPath (curDirsPath directories) (dirs directories)
  case getFileWithPath (getPathByName fileNameToCopy directories) (curFiles dir) of
    Nothing   -> throwError $ NullFileException fileNameToCopy
    Just file -> do
      let (FileContent curFileContent) = fileContent file
      createFileWithContent newFileName (toString curFileContent)
