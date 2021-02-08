module FSExample
  ( dirA
  , dirAWith1
  , dirB
  , dirB1
  , dirB2
  , dirC
  , dirD
  , fileB
  ) where

import Data.ByteString.UTF8 (fromString)
import FileSystem (defPerms, defTime)
import FSTypes (Directories (..), Directory (..), DirInfo (..), File (..), FileContent (..),
               FileInfo(..))


-- | Directory dirA
dirA :: Directory
dirA = Directory
  { curFiles =
    [ File
        { fileContent = FileContent $ fromString ""
        , fileInfo    = FileInfo
          { filePath = "user/name/dirA/fileA1.txt"
          , filePerm = defPerms
          , fileType = ".txt"
          , fileTime = defTime
          , fileSize = 0
          }
        }
      ,
      File
        { fileContent = FileContent $ fromString "I am fileA2!!"
        , fileInfo    = FileInfo
          { filePath = "user/name/dirA/fileA2.txt"
          , filePerm = defPerms
          , fileType = ".txt"
          , fileTime = defTime
          , fileSize = 8 * 13
          }
        }
    ]
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA"
    , filesNumber = 2
    , dirSize     = 8 * 13
    , dirPerm     = defPerms
    }
  }

-- | Directory dirA with only 1 File
dirAWith1 :: Directory
dirAWith1 = Directory
  { curFiles =
    [
      File
        { fileContent = FileContent $ fromString "I am fileA2!!"
        , fileInfo    = FileInfo
          { filePath = "user/name/dirA/fileA2.txt"
          , filePerm = defPerms
          , fileType = ".txt"
          , fileTime = defTime
          , fileSize = 8 * 13
          }
        }
    ]
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA"
    , filesNumber = 2
    , dirSize     = 8 * 13
    , dirPerm     = defPerms
    }
  }

-- | Directory dirB
dirB :: Directory
dirB = Directory
  { curFiles = [
    fileB
  ]
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA/dirB"
    , filesNumber = 1
    , dirSize     = 8 * 17
    , dirPerm     = defPerms
    }
  }

-- | Directory dirB but with edited file
dirB1 :: Directory
dirB1 = Directory
  { curFiles =
    [
      File
        { fileContent = FileContent $ fromString "edit text in dirB"
        , fileInfo    = FileInfo
          { filePath = "user/name/dirA/dirB/fileB.txt"
          , filePerm = defPerms
          , fileType = ".txt"
          , fileTime = defTime
          , fileSize = 8 * 17
          }
        }
    ]
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA/dirB"
    , filesNumber = 1
    , dirSize     = 8 * 17
    , dirPerm     = defPerms
    }
  }

-- | Directory dirB but with another file
dirB2 :: Directory
dirB2 = Directory
  { curFiles = [
    File
      { fileContent = FileContent $ fromString "I am file in dirB"
      , fileInfo    = FileInfo
        { filePath = "user/name/dirA/dirB/fileBcopy.txt"
        , filePerm = defPerms
        , fileType = ".txt"
        , fileTime = defTime
        , fileSize = 8 * 17
        }
      }
      , fileB
  ]
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA/dirB"
    , filesNumber = 1
    , dirSize     = 8 * 17
    , dirPerm     = defPerms
    }
  }

-- | File fileB
fileB :: File
fileB = File
  { fileContent = FileContent $ fromString "I am file in dirB"
  , fileInfo    = FileInfo
    { filePath = "user/name/dirA/dirB/fileB.txt"
    , filePerm = defPerms
    , fileType = ".txt"
    , fileTime = defTime
    , fileSize = 8 * 17
    }
  }

-- | Directory dirC
dirC :: Directory
dirC = Directory
  { curFiles = []
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA/dirC"
    , filesNumber = 0
    , dirSize     = 0
    , dirPerm     = defPerms
    }
  }

-- | Directory dirD
dirD :: Directory
dirD = Directory
  { curFiles = []
  , dirInfo  = DirInfo
    { dirPath     = "user/name/dirA/dirB/dirD"
    , filesNumber = 0
    , dirSize     = 0
    , dirPerm     = defPerms
    }
  }
