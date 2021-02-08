module Main where

import FSInit (readFileSystem)
import FSSave (saveChanges)
import FSTypes (Directories (..))
import FileSystem (applyCommand, showColorText)
import FSException (FileSystemException(..))
import Control.Exception (catch)
import System.Exit (exitFailure)
import Parser (Command (..), parseCommand)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Options.Applicative (ParserResult (..), renderFailure)
import Rainbow (cyan, green, red)
import qualified System.Process as SP


-- | This is kind of a hard(old) version of hw, please read README.md

-- | Entry point to the file system
main :: IO ()
main = do
  _ <- SP.system "reset"
  showColorText cyan showWelcomeMessage
  putStrLn "Press 'ENTER' to use current directory as root or type absolute path"
  root <- getLine
  directories <- catch (readFileSystem root)
    (\e -> do
      hPutStrLn stderr $ show (e :: FileSystemException)
      exitFailure
    )
  showColorText green ("Root successfully set to " ++ (curDirsPath directories))
  newFS <- doCommands directories
  saveChanges directories newFS

-- | Apply given command and returns new directories in infinity loop
doCommands
  :: Directories  -- ^ Current directories
  -> IO (Directories)  -- ^ Directories after command applying
doCommands directories = do
  putStr ((curDirsPath directories) ++ "> ") >> hFlush stdout
  command <- getLine
  case parseCommand command of
    Success EXIT -> do
      showColorText green "Successfully exit"
      return directories
    Success result -> do
      dirs <- applyCommand result directories
      doCommands dirs
    Failure e -> do
      showColorText red $ fst $ renderFailure e ""
      doCommands directories

-- | Welcome text
showWelcomeMessage :: String
showWelcomeMessage =
  "        ／＞_ フ\n" ++
  "　　　　|  _　 _|  Welcome\n" ++
  "　　　／`ミ _x 彡    to\n" ++
  " 　 /　　　 　 |    File\n" ++
  "　 /　 ヽ　　 ﾉ    Manager\n" ++
  "／￣|　　 | | \\\n" ++
  "| (￣ヽ＿_ヽ_)_)\n" ++
  "＼二つ\n\n"
