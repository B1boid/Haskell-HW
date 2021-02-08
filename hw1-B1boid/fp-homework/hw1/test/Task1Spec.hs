module Task1Spec where

import Test.Hspec

import Task1

spec :: Spec
spec = do
  describe "nextDay" $
    it "returns the next day" $ do
      nextDay Sun `shouldBe` Mon
      nextDay Mon `shouldBe` Tue
      nextDay Tue `shouldBe` Wed
      nextDay Wed `shouldBe` Thu
      nextDay Thu `shouldBe` Fri
      nextDay Fri `shouldBe` Sat
      nextDay Sat `shouldBe` Sun

  describe "afterDays" $
    it "returns the day after given days interval" $ do
      afterDays 0 Mon `shouldBe` Mon
      afterDays 1 Mon `shouldBe` Tue
      afterDays 2 Mon `shouldBe` Wed
      afterDays 3 Mon `shouldBe` Thu
      afterDays 4 Mon `shouldBe` Fri
      afterDays 5 Mon `shouldBe` Sat
      afterDays 6 Mon `shouldBe` Sun
      afterDays 7 Mon `shouldBe` Mon
      afterDays 8 Mon `shouldBe` Tue

      afterDays (-1) Mon `shouldBe` Sun
      afterDays (-2) Mon `shouldBe` Sat
      afterDays (-7) Mon `shouldBe` Mon
      afterDays (-8) Mon `shouldBe` Sun

  describe "isWeekend" $
    it "checks if the day is on the weekend" $ do
      isWeekend Mon `shouldBe` False
      isWeekend Tue `shouldBe` False
      isWeekend Wed `shouldBe` False
      isWeekend Thu `shouldBe` False
      isWeekend Fri `shouldBe` False
      isWeekend Sat `shouldBe` True
      isWeekend Sun `shouldBe` True

  describe "daysToParty" $
    it "returns the amount of days to party" $ do
      daysToParty Mon `shouldBe` 4
      daysToParty Tue `shouldBe` 3
      daysToParty Wed `shouldBe` 2
      daysToParty Thu `shouldBe` 1
      daysToParty Fri `shouldBe` 0
      daysToParty Sat `shouldBe` 6
      daysToParty Sun `shouldBe` 5
