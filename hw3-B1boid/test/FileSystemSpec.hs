module FileSystemSpec
  ( spec
  ) where

import Test.Hspec (Spec (..), describe, it, shouldBe)
import Data.ByteString.UTF8 (fromString)
import FileSystem (applyCommand, getMessage, dir, ls, cat, find, info, infodir)
import FSTypes (Directories (..), Directory (..), DirInfo (..), File (..), FileContent (..),
               FileInfo(..))
import FSException (FileSystemException (..))
import Parser (Command (..))
import FSExample (dirA, dirAWith1, dirB, dirB1, dirB2, dirC, dirD, fileB)


spec :: Spec
spec = do
  describe "Functionality 'cd'" $ do
    it "Success: cd" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      let dirsAfter = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      newDirs <- applyCommand (CD "dirB") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Success: cd" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      let dirsAfter = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (CD "..") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: cd" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (CD "dirD") dirsBefore
      newDirs `shouldBe` dirsBefore

  describe "Functionality 'dir'" $ do
    it "Success: dir" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage dir curDirs `shouldBe` "|--dirC\n|--dirB\n|-fileA1.txt\n|-fileA2.txt\n"

  describe "Functionality 'ls'" $ do
    it "Success: ls" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage (ls "dirB") curDirs `shouldBe` "|--dirD\n|-fileB.txt\n"
    it "Failure: ls" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage (ls "dirD") curDirs `shouldBe` (show $ NullDirectoryException "dirD")

  describe "Functionality 'mkdir'" $ do
    it "Success: mkdir" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirD], curDirsPath="user/name/dirA"}
      let dirsAfter = Directories{dirs=[dirC, dirA, dirB, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (MKDIR "dirC") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: mkdir" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (MKDIR "dirB") dirsBefore
      newDirs `shouldBe` dirsBefore

  describe "Functionality 'cat'" $ do
    it "Success: cat" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      getMessage (cat "fileB.txt") curDirs `shouldBe` show (fileContent fileB)
    it "Failure: cat" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      getMessage (cat "fileB2.txt") curDirs `shouldBe` (show $ NullFileException "fileB2.txt")

  describe "Functionality 'touch'" $ do
    it "Success: touch" $ do
      let dirsBefore = Directories{dirs=[dirAWith1, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      let dirsAfter = Directories{dirs=[dirA, dirD, dirC, dirB], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (TOUCH "fileA1.txt") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: touch" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (TOUCH "fileA1.txt") dirsBefore
      newDirs `shouldBe` dirsBefore

  describe "Functionality 'rmdir'" $ do
    it "Success: rmdir" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      let dirsAfter = Directories{dirs=[dirC, dirA], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (RMDIR "dirB") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: rmdir" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (RMDIR "dirD") dirsBefore
      newDirs `shouldBe` dirsBefore

  describe "Functionality 'rm'" $ do
    it "Success: rm" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      let dirsAfter = Directories{dirs=[dirAWith1, dirD, dirC, dirB], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (RM "fileA1.txt") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: rm" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      newDirs <- applyCommand (RM "fileA3.txt") dirsBefore
      newDirs `shouldBe` dirsBefore

  describe "Functionality 'edit'" $ do
    it "Success: edit" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      let dirsAfter = Directories{dirs=[dirB1, dirD, dirC, dirA], curDirsPath="user/name/dirA/dirB"}
      newDirs <- applyCommand (EDIT "fileB.txt" ["edit", "text", "in", "dirB"]) dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: edit" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      newDirs <- applyCommand (EDIT "fileB2.txt" ["new", "text"]) dirsBefore
      newDirs `shouldBe` dirsBefore

  describe "Functionality 'find'" $ do
    it "Success: find" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage (find "fileB.txt") curDirs `shouldBe` "-> user/name/dirA/dirB/fileB.txt\n"
    it "Failure: find" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage (find "fileB2.txt") curDirs `shouldBe` (show $ NullFileException "fileB2.txt")

  describe "Functionality 'info'" $ do
    it "Success: info" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      getMessage (info "fileB.txt") curDirs `shouldBe` (show $ fileInfo fileB)
    it "Failure: info" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      getMessage (info "fileB2.txt") curDirs `shouldBe` (show $ NullFileException "fileB2.txt")

  describe "Functionality 'infodir'" $ do
    it "Success: infodir" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage (infodir "dirB") curDirs `shouldBe` (show $ dirInfo dirB)
    it "Failure: infodir" $ do
      let curDirs = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA"}
      getMessage (infodir "dirE") curDirs `shouldBe` (show $ NullDirectoryException "dirE")

  describe "Functionality 'copy'" $ do
    it "Success: copy" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      let dirsAfter = Directories{dirs=[dirB2, dirD, dirC, dirA], curDirsPath="user/name/dirA/dirB"}
      newDirs <- applyCommand (COPY "fileB.txt" "fileBcopy.txt") dirsBefore
      newDirs `shouldBe` dirsAfter
    it "Failure: copy" $ do
      let dirsBefore = Directories{dirs=[dirA, dirB, dirC, dirD], curDirsPath="user/name/dirA/dirB"}
      newDirs <- applyCommand (COPY "fileB2.txt" "fileBcopy.txt") dirsBefore
      newDirs `shouldBe` dirsBefore
