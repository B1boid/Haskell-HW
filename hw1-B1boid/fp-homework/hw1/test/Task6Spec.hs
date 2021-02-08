module Task6Spec where

import Test.Hspec
import Task6

spec :: Spec
spec = do
  describe "maybeConcat" $
    it "returns concatenation of unwrapped 'Maybe' elements of given list" $ do
      maybeConcat []                                  `shouldBe` ([] :: [Int])
      maybeConcat [Nothing]                           `shouldBe` ([] :: [Int])
      maybeConcat [Just [1]]                          `shouldBe` [1]
      maybeConcat [Just [1, 2, 3]]                    `shouldBe` [1, 2, 3]
      maybeConcat [Just [1, 2], Just [3], Just [4]]   `shouldBe` [1, 2, 3, 4]
      maybeConcat [Just [1, 2], Nothing, Just [3, 4]] `shouldBe` [1, 2, 3, 4]

  describe "HardV: eitherConcat" $
    it "returns concatenation of unwrapped 'Either' separately" $ do
      eitherConcat [Left [0], Right [1]] `shouldBe` ([0], [1])
      eitherConcat [Left [0],
                    Left [1, 2],
                    Right [5],
                    Right [1, 4],
                    Left [7, 7]]  `shouldBe` ([0, 1, 2, 7, 7], [5, 1, 4])
      eitherConcat [Left [1, 2],
                    Left [7, 7]]  `shouldBe` ([1, 2, 7, 7], [] :: [Int])
      eitherConcat [Left (Just [0, 1, 2]),
                    Right [1, 1],
                    Left Nothing,
                    Right [4, 5]] `shouldBe` (Just [0, 1, 2], [1, 1, 4, 5])
