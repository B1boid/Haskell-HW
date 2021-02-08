module Task3and4Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List.NonEmpty hiding (insert, toList)
import Task3and4
import Data.List hiding (find, insert)

treeFromList = Task3and4.fromList . Data.List.NonEmpty.fromList

toList :: Ord a => Tree a -> [a]
toList = foldr (:) []

spec :: Spec
spec = do
  describe "isEmpty, treeSize and find" $ do
    it "checks if the 'Tree' is empty" $ do
      isEmpty Leaf `shouldBe` True
      isEmpty (treeFromList [0])         `shouldBe` False
      isEmpty (treeFromList [3])         `shouldBe` False
      isEmpty (treeFromList [1, 2, 3])   `shouldBe` False
      isEmpty (Node Leaf (4 :| []) Leaf) `shouldBe` False

    it "returns size of the 'Tree'" $ do
      treeSize Leaf                           `shouldBe` 0
      treeSize (Node Leaf (1 :| []) Leaf)     `shouldBe` 1
      treeSize (treeFromList [0])             `shouldBe` 1
      treeSize (treeFromList [1, 2, 3])       `shouldBe` 3
      treeSize (treeFromList [1, 1, 1, 1])    `shouldBe` 4
      treeSize (treeFromList [0, 3, 1, 2, 2]) `shouldBe` 5

    it "checks if the element is in the 'Tree'" $ do
      find Leaf                           0 `shouldBe` False
      find (treeFromList [1, 2, 3])       0 `shouldBe` False
      find (treeFromList [1, 2, 3])       1 `shouldBe` True
      find (treeFromList [1, 1, 1])       1 `shouldBe` True
      find (treeFromList [4, 3, 2, 5, 6]) 2 `shouldBe` True

  describe "insert and remove" $ do
    it "returns the 'Tree' with given inserted element" $ do
      toList (insert Leaf                     1)    `shouldBe` [1]
      toList (insert (treeFromList [1, 2, 3]) 0)    `shouldBe` [0, 1, 2, 3]
      toList (insert (treeFromList [3, 2, 1]) 0)    `shouldBe` [0, 1, 2, 3]
      toList (insert (treeFromList [1, 1, 1]) 1)    `shouldBe` [1, 1, 1, 1]
      toList (insert (treeFromList [3, 5, 6]) 4)    `shouldBe` [3, 4, 5, 6]
      toList (insert (treeFromList [1, 1, 2, 2]) 5) `shouldBe` [1, 1, 2, 2, 5]

    it "returns the 'Tree' without given removed element" $ do
      toList (remove Leaf                     1)    `shouldBe` []
      toList (remove (treeFromList [1, 1, 1]) 1)    `shouldBe` []
      toList (remove (treeFromList [1, 2, 3]) 0)    `shouldBe` [1, 2, 3]
      toList (remove (treeFromList [1, 2, 3]) 1)    `shouldBe` [2, 3]
      toList (remove (treeFromList [3, 5, 6, 6]) 5) `shouldBe` [3, 6, 6]

  describe "fromList" $ do
    it "returns 'Tree' from given non empty list" $ do
      toList (treeFromList [1])          `shouldBe` [1]
      toList (treeFromList [1, 1, 1])    `shouldBe` [1, 1, 1]
      toList (treeFromList [1, 2, 3])    `shouldBe` [1, 2, 3]
      toList (treeFromList [3, 2, 1])    `shouldBe` [1, 2, 3]
      toList (treeFromList [1, 7, 4, 2]) `shouldBe` [1, 2, 4, 7]

  describe "foldr,foldMap" $ do
    it "foldr for the 'Tree'" $ do
      foldr (+) 3 (treeFromList [2])       `shouldBe` 5
      foldr (+) 1 (treeFromList [1, 2, 3]) `shouldBe` 7
      foldr (*) 2 (treeFromList [3, 4, 5]) `shouldBe` 120

    it "returns 'Tree' from given non empty list" $ do
      foldMap (\x -> [x + 1]) (treeFromList [1, 2])      `shouldBe` [2, 3]
      foldMap (\x -> show x) (treeFromList [1, 2, 3, 4]) `shouldBe` "1234"

  describe "foldr,foldMap" $ do
    prop "PROP: Foldable" $ \arg1 ->
      (toList .treeFromList) ((arg1 :: [Int]) ++ [1]) `shouldBe`
       Data.List.sort (arg1 ++ [1])
