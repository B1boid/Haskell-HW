module Block2Spec where

import Test.Hspec

import Block2

spec :: Spec
spec = do
  describe "Block2" $ do
    it "arithmetic expression" $ do
      eval (Op Add (Const 1) (Const 2))                    `shouldBe` Right 3
      eval (Op Sub (Const 1) (Const 2))                    `shouldBe` Right (-1)
      eval (Op Mul (Const 6) (Const 2))                    `shouldBe` Right 12
      eval (Op Div (Const 6) (Const 2))                    `shouldBe` Right 3
      eval (Op Pow (Const 6) (Const 2))                    `shouldBe` Right 36
      eval (Op Pow (Const 6) (Const 0))                    `shouldBe` Right 1
      eval (Op Add (Op Mul (Const 4) (Const 2)) (Const 5)) `shouldBe` Right 13
      eval (Op Div (Op Sub (Const 9) (Const 2)) (Const 5)) `shouldBe` Right 1
      eval (Op Add (Op Add (Const 4) (Const 2))
                   (Op Pow (Const 4) (Const 3)))           `shouldBe` Right 70

      eval (Op Div (Const 1) (Const 0))    `shouldBe` Left DivisionByZero
      eval (Op Div (Const 1) (Op Add (Const (-2)) (Const 2)))
                                           `shouldBe` Left DivisionByZero
      eval (Op Pow (Const 1) (Const (-1))) `shouldBe` Left NegativePow
      eval (Op Pow (Const 5) (Op Add (Const (-3)) (Const 2)))
                                           `shouldBe` Left NegativePow

    it "moving" $ do
      moving 1 []  `shouldBe` []
      moving 2 [1] `shouldBe` [1.0]
      moving 1 [1] `shouldBe` [1.0]
      moving 4 [1, 5, 3, 8, 7, 9, 6]
                   `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
      moving 1 [1, 5, 3, 8, 7, 9, 6]
                   `shouldBe` [1.0, 5.0, 3.0, 8.0, 7.0, 9.0, 6.0]
      moving 2 [3, 1, 4, 5]
                   `shouldBe` [3.0, 2.0, 2.5, 4.5]
