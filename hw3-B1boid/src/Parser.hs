module Parser
  ( Command(..)

  , parseCommand
  , helpText
  ) where

import Options.Applicative(Parser (..), ParserInfo (..), ParserResult (..), command, some, metavar,
                          idm, info, progDesc, subparser, strArgument,defaultPrefs, execParserPure,
                          renderFailure)

-- | Data type for file system commands
data Command
  = CD String  -- ^ Change directory to chosen one
  | DIR  -- ^ Show content of current directory
  | LS String  -- ^ Show content of chosen directory
  | MKDIR String  -- ^ Create folder in current directory
  | CAT String  -- ^ Show content of chosen file
  | TOUCH String  -- ^ Create file in current directory
  | RMDIR String  -- ^ Delete directory with chosen name
  | RM String  -- ^ Delete file with chosen name
  | EDIT String [String]  -- ^ Write to file new text
  | FIND String  -- ^ Find file in current directory and subdirectories
  | INFO String  -- ^ Show information about this file
  | INFODIR String  -- ^ Show information about this directory
  | HELP  -- ^ Show help guide
  | EXIT  -- ^ Save all changes in real file system and exit
  | CLR  -- ^ Clear the terminal
  | COPY String String  -- ^ Copy file and create new with the same content
  deriving (Show, Eq)

-- | Parse given string to command
parseCommand
  :: String  -- ^ Given string with command
  -> ParserResult Command  -- ^ Parsed command
parseCommand command = execParserPure defaultPrefs (info cmdParser idm) (words command)

-- | Parser for command
cmdParser :: Parser Command
cmdParser = subparser
          ( command "cd"
            (info (CD <$> strArgument (metavar "DIRECTORY")) (progDesc $ getInfo "cd"))
         <> command "dir"
            (info (pure DIR) (progDesc $ getInfo "dir"))
         <> command "ls"
            (info (LS <$> strArgument (metavar "DIRECTORY")) (progDesc $ getInfo "ls"))
         <> command "mkdir"
            (info (MKDIR <$> strArgument (metavar "DIRECTORY")) (progDesc $ getInfo "mkdir"))
         <> command "cat"
            (info (CAT <$> strArgument (metavar "FILE")) (progDesc $ getInfo "cat"))
         <> command "touch"
            (info (TOUCH <$> strArgument (metavar "FILE")) (progDesc $ getInfo "touch"))
         <> command "rmdir"
            (info (RMDIR <$> strArgument (metavar "DIRECTORY")) (progDesc $ getInfo "rmdir"))
         <> command "rm"
            (info (RM <$> strArgument (metavar "FILE")) (progDesc $ getInfo "rm"))
         <> command "edit"
            (info (EDIT <$> strArgument (metavar "FILE") <*> some (strArgument (metavar "TEXT")))
            (progDesc $ getInfo "edit"))
         <> command "find"
            (info (FIND <$> strArgument (metavar "FILE")) (progDesc $ getInfo "find"))
         <> command "info"
            (info (INFO <$> strArgument (metavar "FILE")) (progDesc $ getInfo "info"))
         <> command "infodir"
            (info (INFODIR <$> strArgument (metavar "DIRECTORY")) (progDesc $ getInfo "infodir"))
         <> command "copy"
            (info (COPY <$> strArgument (metavar "FILE") <*> strArgument (metavar "FILE"))
            (progDesc $ getInfo "copy"))
         <> command "help"
            (info (pure HELP) (progDesc $ getInfo "help"))
         <> command "exit"
            (info (pure EXIT) (progDesc $ getInfo "exit"))
         <> command "clr"
            (info (pure CLR) (progDesc $ getInfo "clr"))
         )

-- | Get description of command
getInfo
  :: String  -- ^ Given command string id
  -> String  -- ^ Returns description of command
getInfo "cd"      = "change directory to chosen one or to parent if (cd ..)"
getInfo "dir"     = "show content of current directory"
getInfo "ls"      = "show content of chosen directory"
getInfo "mkdir"   = "create folder in current directory"
getInfo "cat"     = "show content of chosen file"
getInfo "help"    = "show this help guide"
getInfo "touch"   = "create file in current directory"
getInfo "rmdir"   = "delete directory with chosen name"
getInfo "rm"      = "delete file with chosen name"
getInfo "edit"    = "write to file new text"
getInfo "find"    = "find file in current directory and subdirectories"
getInfo "info"    = "show information about this file"
getInfo "infodir" = "show information about this directory"
getInfo "exit"    = "save all changes in real file system and exit"
getInfo "clr"     = "clear the terminal"
getInfo "copy"    = "copy first file and create new with the same content"

-- | Help text
helpText :: String
helpText =
       "\nAvailable commands:"
    ++ "\n help                    -- " ++ (getInfo "help")
    ++ "\n cd      <folder>        -- " ++ (getInfo "cd")
    ++ "\n dir                     -- " ++ (getInfo "dir")
    ++ "\n ls      <folder>        -- " ++ (getInfo "ls")
    ++ "\n mkdir   <folder>        -- " ++ (getInfo "mkdir")
    ++ "\n cat     <file>          -- " ++ (getInfo "cat")
    ++ "\n touch   <file>          -- " ++ (getInfo "touch")
    ++ "\n rmdir   <folder>        -- " ++ (getInfo "rmdir")
    ++ "\n rm      <file>          -- " ++ (getInfo "rm")
    ++ "\n edit    <file>  <text>  -- " ++ (getInfo "edit")
    ++ "\n find    <file>          -- " ++ (getInfo "find")
    ++ "\n info    <file>          -- " ++ (getInfo "info")
    ++ "\n infodir <folder>        -- " ++ (getInfo "infodir")
    ++ "\n exit                    -- " ++ (getInfo "exit")
    ++ "\n clr                     -- " ++ (getInfo "clr")
    ++ "\n copy    <file>  <file>  -- " ++ (getInfo "copy")
