module Task1Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Task1 (Point (..), perimeter, doubleArea, perimeterNaive, doubleAreaNaive)


-- | Unit tests for Task1
spec :: Spec
spec = do
  describe "Task1" $ do
    it "Perimeter" $ do
      perimeter [] `shouldBe` 0
      perimeter [(Point 1 1)] `shouldBe` 0
      perimeter [(Point 0 0), (Point 0 3), (Point 4 0)] `shouldBe` 12
      perimeter [(Point 0 0), (Point 0 2), (Point 2 2), (Point 2 0)] `shouldBe` 8
      perimeter [(Point 0 0), (Point 0 1), (Point 3 5), (Point 3 1), (Point 3 0)] `shouldBe` 14

    it "Double area" $ do
      doubleArea [] `shouldBe` 0
      doubleArea [(Point 1 1)] `shouldBe` 0
      doubleArea [(Point 0 0), (Point 0 3), (Point 4 0)] `shouldBe` 12
      doubleArea [(Point 0 0), (Point 0 2), (Point 2 2), (Point 2 0)] `shouldBe` 8
      doubleArea [(Point 0 0), (Point 0 1), (Point 3 5), (Point 3 1), (Point 3 0)] `shouldBe` 18

    it "Perimeter naive" $ do
      perimeterNaive [] `shouldBe` 0
      perimeterNaive [(Point 1 1)] `shouldBe` 0
      perimeterNaive [(Point 0 0), (Point 0 3), (Point 4 0)] `shouldBe` 12
      perimeterNaive [(Point 0 0), (Point 0 2), (Point 2 2), (Point 2 0)] `shouldBe` 8
      perimeterNaive [(Point 0 0), (Point 0 1), (Point 3 5), (Point 3 1), (Point 3 0)]
        `shouldBe` 14

    it "Double area naive" $ do
      doubleAreaNaive [] `shouldBe` 0
      doubleAreaNaive [(Point 1 1)] `shouldBe` 0
      doubleAreaNaive [(Point 0 0), (Point 0 3), (Point 4 0)] `shouldBe` 12
      doubleAreaNaive [(Point 0 0), (Point 0 2), (Point 2 2), (Point 2 0)] `shouldBe` 8
      doubleAreaNaive [(Point 0 0), (Point 0 1), (Point 3 5), (Point 3 1), (Point 3 0)]
        `shouldBe` 18
