module Task7Spec where

import Test.Hspec
import Task7

spec :: Spec
spec = do
  describe "NonEmpty and ThisOrThat" $ do
    it "checks semigroup conditions for 'NonEmpty'" $ do
      (0 :| [])     <> (1 :| [])     `shouldBe` (0 :| [1])
      (0 :| [1])    <> (2 :| [3])    `shouldBe` (0 :| [1, 2, 3])
      (0 :| [1, 4]) <> (2 :| [3, 5]) `shouldBe` (0 :| [1, 4, 2, 3, 5])

      ((0 :| [1]) <> (2 :| [3])) <> (4 :| [5]) `shouldBe` (0 :| [1, 2, 3, 4, 5])
      (0 :| [1]) <> ((2 :| [3]) <> (4 :| [5])) `shouldBe` (0 :| [1, 2, 3, 4, 5])

    it "checks semigroup conditions for 'ThisOrThat'" $ do
      This 1 <> This 2     `shouldBe` (This 1 :: ThisOrThat Int Int)
      This 1 <> That 2     `shouldBe` Both 1 2
      This 1 <> Both 2 3   `shouldBe` Both 1 3
      That 2 <> This 1     `shouldBe` Both 1 2
      That 1 <> That 2     `shouldBe` (That 1 :: ThisOrThat Int Int)
      That 1 <> Both 2 3   `shouldBe` Both 2 1
      Both 1 2 <> This 3   `shouldBe` Both 1 2
      Both 1 2 <> That 3   `shouldBe` Both 1 2
      Both 1 2 <> Both 3 4 `shouldBe` Both 1 2

      (This 1 <> That 2)   <> This 3  `shouldBe` Both 1 2
      This 1  <> (That 2   <> This 3) `shouldBe` Both 1 2
      (That 1 <> Both 2 3) <> This 4  `shouldBe` Both 2 1
      That 1  <> (Both 2 3 <> This 4) `shouldBe` Both 2 1

  describe "HardV: Name and Endo semigroup,monoid" $ do
    it "checks Name conditions" $ do
      EmptyName    <> EmptyName                `shouldBe` EmptyName
      Name "First" <> EmptyName                `shouldBe` Name "First"
      EmptyName    <> Name "Last"              `shouldBe` Name "Last"
      Name "root"  <> Name "server"            `shouldBe` Name "root.server"
      (Name "aa"   <> Name "bb") <> Name "cc"  `shouldBe` Name "aa.bb.cc"
      Name "aa"    <> (Name "bb" <> Name "cc") `shouldBe` Name "aa.bb.cc"

    it "checks Endo conditions" $ do
      getEndo (Endo (* 2) <> mempty) 5 `shouldBe` getEndo (Endo (* 2)) 5
      getEndo (mempty <> Endo (* 2)) 5 `shouldBe` getEndo (Endo (* 2)) 5

      getEndo ((Endo (* 2) <> Endo (+ 3)) <> Endo (* 4))    5 `shouldBe`
        getEndo (Endo (* 2) <> (Endo (+ 3) <> Endo (* 4)))  5
      getEndo ((Endo (10 -) <> Endo (* 2)) <> Endo (+ 1))   2 `shouldBe`
        getEndo (Endo (10 -) <> (Endo (* 2) <> Endo (+ 1))) 2
