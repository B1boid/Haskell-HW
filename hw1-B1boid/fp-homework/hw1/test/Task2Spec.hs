module Task2Spec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Task2

spec :: Spec
spec = do
  describe "Conversion Nat <-> Int" $ do
    it "Nat to Int" $ do
      toInt Z                         `shouldBe` 0
      toInt (S Z)                     `shouldBe` 1
      toInt (S (S Z))                 `shouldBe` 2
      toInt (S (S (S (S (S (S Z)))))) `shouldBe` 6

    it "Int to Nat" $ do
      fromInt 0    `shouldBe` Z
      fromInt (-4) `shouldBe` Z
      fromInt 1    `shouldBe` S Z
      fromInt 2    `shouldBe` S (S Z)
      fromInt 6    `shouldBe` (S (S (S (S (S (S Z))))))

  describe "Nat operations: + * -" $ do
    it "Sum" $ do
      natAdd (fromInt 0) (fromInt 0) `shouldBe` (fromInt 0)
      natAdd (fromInt 1) (fromInt 0) `shouldBe` (fromInt 1)
      natAdd (fromInt 0) (fromInt 3) `shouldBe` (fromInt 3)
      natAdd (fromInt 3) (fromInt 5) `shouldBe` (fromInt 8)
      natAdd (fromInt 5) (fromInt 3) `shouldBe` (fromInt 8)
      natAdd (fromInt 9) (fromInt 9) `shouldBe` (fromInt 18)

    prop "PROP: Sum" $ \arg1 arg2 ->
      natAdd (fromInt (abs arg1)) (fromInt (abs arg2)) `shouldBe`
       (fromInt ((abs arg1) + (abs arg2)))

    it "Multiplication" $ do
      natMul (fromInt 0) (fromInt 0) `shouldBe` (fromInt 0)
      natMul (fromInt 1) (fromInt 0) `shouldBe` (fromInt 0)
      natMul (fromInt 0) (fromInt 3) `shouldBe` (fromInt 0)
      natMul (fromInt 3) (fromInt 5) `shouldBe` (fromInt 15)
      natMul (fromInt 5) (fromInt 3) `shouldBe` (fromInt 15)
      natMul (fromInt 9) (fromInt 9) `shouldBe` (fromInt 81)

    prop "PROP: Multiplication" $ \arg1 arg2 ->
      natMul (fromInt (abs arg1)) (fromInt (abs arg2)) `shouldBe`
       (fromInt ((abs arg1) * (abs arg2)))

    it "Subtraction" $ do
      natSub (fromInt 0) (fromInt 0) `shouldBe` (fromInt 0)
      natSub (fromInt 1) (fromInt 0) `shouldBe` (fromInt 1)
      natSub (fromInt 0) (fromInt 3) `shouldBe` (fromInt 0)
      natSub (fromInt 3) (fromInt 5) `shouldBe` (fromInt 0)
      natSub (fromInt 5) (fromInt 3) `shouldBe` (fromInt 2)
      natSub (fromInt 9) (fromInt 9) `shouldBe` (fromInt 0)

    prop "PROP: Subtraction" $ \arg1 arg2 ->
      natSub (fromInt (abs arg1)) (fromInt (abs arg2)) `shouldBe`
       (fromInt ((abs arg1) - (abs arg2)))

  describe "Nat equality and comparison" $ do
    it "Equality" $ do
      (fromInt 0) == (fromInt 0) `shouldBe` True
      (fromInt 1) == (fromInt 1) `shouldBe` True
      (fromInt 5) == (fromInt 5) `shouldBe` True
      (fromInt 0) == (fromInt 1) `shouldBe` False
      (fromInt 2) == (fromInt 0) `shouldBe` False
      (fromInt 3) == (fromInt 5) `shouldBe` False

    prop "PROP: Equality" $ \arg1 arg2 ->
      ((fromInt (abs arg1)) == (fromInt (abs arg2))) `shouldBe`
       ((abs arg1) == (abs arg2))

    it "Comparison" $ do
      compare (fromInt 0) (fromInt 0) `shouldBe` EQ
      compare (fromInt 1) (fromInt 1) `shouldBe` EQ
      compare (fromInt 5) (fromInt 5) `shouldBe` EQ
      compare (fromInt 1) (fromInt 0) `shouldBe` GT
      compare (fromInt 5) (fromInt 3) `shouldBe` GT
      compare (fromInt 0) (fromInt 1) `shouldBe` LT
      compare (fromInt 3) (fromInt 5) `shouldBe` LT

    prop "PROP: Comparison" $ \arg1 arg2 ->
      compare (fromInt (abs arg1)) (fromInt (abs arg2)) `shouldBe`
       (compare (abs arg1) (abs arg2))

  describe "HardV: div, mod and isEven" $ do
    it "Division" $ do
      natDiv (fromInt 0) (fromInt 1) `shouldBe` (fromInt 0)
      natDiv (fromInt 0) (fromInt 3) `shouldBe` (fromInt 0)
      natDiv (fromInt 3) (fromInt 1) `shouldBe` (fromInt 3)
      natDiv (fromInt 3) (fromInt 2) `shouldBe` (fromInt 1)
      natDiv (fromInt 3) (fromInt 3) `shouldBe` (fromInt 1)
      natDiv (fromInt 4) (fromInt 3) `shouldBe` (fromInt 1)
      natDiv (fromInt 9) (fromInt 2) `shouldBe` (fromInt 4)

    prop "PROP: Division" $ \arg1 arg2 ->
      natDiv (fromInt (abs arg1)) (fromInt ((abs arg2) + 1)) `shouldBe`
        fromInt ((abs arg1) `div` ((abs arg2) + 1))

    it "Modulo" $ do
      natMod (fromInt 0) (fromInt 1) `shouldBe` (fromInt 0)
      natMod (fromInt 0) (fromInt 3) `shouldBe` (fromInt 0)
      natMod (fromInt 3) (fromInt 1) `shouldBe` (fromInt 0)
      natMod (fromInt 3) (fromInt 2) `shouldBe` (fromInt 1)
      natMod (fromInt 3) (fromInt 3) `shouldBe` (fromInt 0)
      natMod (fromInt 4) (fromInt 5) `shouldBe` (fromInt 4)
      natMod (fromInt 9) (fromInt 2) `shouldBe` (fromInt 1)

    prop "PROP: Modulo" $ \arg1 arg2 ->
      natMod (fromInt (abs arg1)) (fromInt ((abs arg2) + 1)) `shouldBe`
        fromInt ((abs arg1) `mod` ((abs arg2) + 1))

    it "isEven" $ do
      isEven (fromInt 0) `shouldBe` True
      isEven (fromInt 2) `shouldBe` True
      isEven (fromInt 8) `shouldBe` True
      isEven (fromInt 1) `shouldBe` False
      isEven (fromInt 3) `shouldBe` False
      isEven (fromInt 5) `shouldBe` False

    prop "PROP: isEven" $ \arg1 ->
      isEven (fromInt (abs arg1)) `shouldBe` arg1 `mod` 2 == 0
