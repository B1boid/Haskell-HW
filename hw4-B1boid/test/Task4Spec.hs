module Task4Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Task4 (runScript, example)

-- | Unit tests for Task4
spec :: Spec
spec = do
  describe "Task4" $ do
    it "Simple example with all types (Int, String, Bool)" $ do
      runScript example `shouldBe` "Hello! Your rank is *****"
