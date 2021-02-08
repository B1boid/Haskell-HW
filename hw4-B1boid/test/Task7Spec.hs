module Task7Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import System.FilePath ((</>))
import Lens.Micro ((^..), (^?))
import Task6 (FS (..), getDirectory')
import Task7 (cd, ls, file)

-- | File system for test
testFileSystem :: FS
testFileSystem = Dir "testExampleDir"
                 [ testSubDir
                 , File "exampleFile"
                 ]
-- | First subdirectory
testSubDir :: FS
testSubDir =  Dir "exampleSubDir" [File "exampleFile2"]

-- | File system path
testFileSystemPath :: FilePath
testFileSystemPath = "test" </> "testExampleDir"

-- | Unit tests for Task7
spec :: Spec
spec = do
  describe "Task7" $ do
    it "CD to invalid directory" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^? cd "nonExistingDir" `shouldBe` Nothing

    it "CD to valid directory" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^? cd "exampleSubDir" `shouldBe` (Just testSubDir)

    it "LS" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^.. ls `shouldBe` ["exampleSubDir", "exampleFile"]

    it "FILE of invalid file" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^? file "nonExistingFile" `shouldBe` Nothing

    it "FILE of valid file" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^? file "exampleFile" `shouldBe` Just "exampleFile"

    it "CD + LS" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^.. cd "exampleSubDir" . ls `shouldBe` ["exampleFile2"]

    it "CD + FILE" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^? cd "exampleSubDir" . file "exampleFile2" `shouldBe` Just "exampleFile2"
