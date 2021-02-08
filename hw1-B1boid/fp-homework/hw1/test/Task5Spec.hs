module Task5Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List.NonEmpty (NonEmpty(..))
import Task5

spec :: Spec
spec = do
  describe "splitOn" $ do
    it "returns NonEmpty list splitted by separator" $ do
      splitOn '/' "path/to/file"   `shouldBe` ("path" :| ["to", "file"])
      splitOn '|' "path|to|file"   `shouldBe` ("path" :| ["to", "file"])
      splitOn '|' "path/to/file"   `shouldBe` ("path/to/file" :| [])
      splitOn '/' "/path/to/file/" `shouldBe` ("" :| ["path", "to", "file", ""])
      splitOn '/' "/"              `shouldBe` ("" :| [""])
      splitOn '/' ""               `shouldBe` ("" :| [])
      splitOn '/' "///"            `shouldBe` ("" :| ["", "", ""])

  describe "HardV: joinWith" $
    it "returns list with separator between elements of given list" $ do
      joinWith '/' ("path" :| ["to", "file"]) `shouldBe` "path/to/file"
      joinWith '|' ("path" :| ["to", "file"]) `shouldBe` "path|to|file"
      joinWith ' ' ("path" :| ["", "file"])   `shouldBe` "path  file"
      joinWith '/' ([] :| [])                 `shouldBe` ""
      joinWith '/' ("" :| ["", ""])           `shouldBe` "//"

  describe "PROP: splitOn and joinWith" $ do
    prop "PROP: splitOn and joinWith" $ \arg1 ->
      (joinWith '/' . splitOn '/') arg1 `shouldBe` arg1
