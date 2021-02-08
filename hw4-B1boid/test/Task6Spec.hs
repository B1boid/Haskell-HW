module Task6Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import System.FilePath ((</>))
import Lens.Micro ((^.))
import Task6 (FS (..), getDirectory', name, contents)


-- | File system for test
testFileSystem :: FS
testFileSystem = Dir "testExampleDir"
                 [ Dir "exampleSubDir" [File "exampleFile2"]
                 , File "exampleFile"
                 ]

-- | File system path
testFileSystemPath :: FilePath
testFileSystemPath = "test" </> "testExampleDir"

-- | Unit tests for Task6
spec :: Spec
spec = do
  describe "Task6" $ do
    it "getDirectory' test" $ do
      directories <- getDirectory' testFileSystemPath
      directories `shouldBe` testFileSystem

    it "lense name" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^. name `shouldBe` _name testFileSystem

    it "lense content" $ do
      directories <- getDirectory' testFileSystemPath
      directories ^. contents `shouldBe` _contents testFileSystem
