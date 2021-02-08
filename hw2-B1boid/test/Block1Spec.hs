module Block1Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Data.Char (isDigit, isSpace)
import Block1 (stringSum)

-- | Make string which stringSum result is Nothing
makeInvalidString
  :: String  -- ^ Random string
  -> String  -- ^ Invalid string
makeInvalidString rndStr = if (resStr == rndStr) || (all isSpace resStr) then "a" else rndStr
  where
    resStr = filter (not . isDigit) rndStr

-- | Unit and Property tests for Block1
spec :: Spec
spec = do
  describe "Block1" $ do
    it "stringSum" $ do
      stringSum "0"            `shouldBe` Just 0
      stringSum "10"           `shouldBe` Just 10
      stringSum "1 2 3"        `shouldBe` Just 6
      stringSum "1   2      3" `shouldBe` Just 6
      stringSum "111 -11"      `shouldBe` Just 100
      stringSum "hey"          `shouldBe` Nothing
      stringSum "1 2 3 hey"    `shouldBe` Nothing
      stringSum "hey 1 2 3"    `shouldBe` Nothing

    prop "Property valid: stringSum" $ \arg1 ->
      stringSum (concatMap (\x -> (show x) ++ " ") (arg1 :: [Int])) `shouldBe` (Just $ sum arg1)

    prop "Property invalid: stringSum" $ \arg1 ->
      stringSum (makeInvalidString (arg1 :: String)) `shouldBe` Nothing
