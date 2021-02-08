{-# LANGUAGE LambdaCase #-}

module Block3Spec where

import Test.Hspec
import Block3
import Control.Applicative (Alternative(..))

f :: Int -> Int
f = (+ 2)

g :: Int -> Int
g = (* 3)

w :: Parser Char Int
w = pure 0

spec :: Spec
spec = do
  describe "Functor instance for Parser" $ do
    it "law: fmap id = id" $ do
      runParser (fmap id parserInt) ""
                `shouldBe` runParser (id parserInt) ""
      runParser (fmap id parserInt) "11"
                `shouldBe` runParser (id parserInt) "11"
      runParser (fmap id parserInt) "-1"
                `shouldBe` runParser (id parserInt) "-1"

    it "law: fmap (f . g) = fmap f . fmap g" $ do
      runParser (fmap (f . g) parserInt) ""
                `shouldBe` runParser (fmap f . fmap g $ parserInt) ""
      runParser (fmap (f . g) parserInt) "11"
                `shouldBe` runParser (fmap f . fmap g $ parserInt) "11"
      runParser (fmap (f . g) parserInt) "-1"
                `shouldBe` runParser (fmap f . fmap g $ parserInt) "-1"

  describe "Applicative instance for Parser" $ do
    it "law: pure id <*> v = v" $ do
      runParser (pure id <*> w) ""   `shouldBe` runParser w ""
      runParser (pure id <*> w) "12" `shouldBe` runParser w "12"

    it "law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      runParser (pure (.) <*> (pure f) <*> (pure g) <*> w) ""
                 `shouldBe` runParser ((pure f) <*> ((pure g) <*> w)) ""
      runParser (pure (.) <*> (pure f) <*> (pure g) <*> w) "12"
                 `shouldBe` runParser ((pure f) <*> ((pure g) <*> w)) "12"

    it "law: pure f <*> pure x = pure (f x)" $ do
      runParser (pure f <*> pure 0) ""   `shouldBe` runParser (pure (f 0)) ""
      runParser (pure f <*> pure 0) "12" `shouldBe` runParser (pure (f 0)) "12"

    it "law: u <*> pure y = pure ($ y) <*> u" $ do
      runParser (pure f <*> w) ""
                           `shouldBe` runParser (pure ($ 0) <*> pure f) ""
      runParser (pure f <*> w) "12"
                           `shouldBe` runParser (pure ($ 0) <*> pure f) "12"

  describe "Monad instance for Parser" $ do
    it "law: return x >>= f = f x" $ do
      runParser (return 0 >>= (return . f)) ""
                            `shouldBe` runParser ((return . f) 0) ""
      runParser (return 0 >>= (return . f)) "12"
                            `shouldBe` runParser ((return . f) 0) "12"

    it "law: m >>= return m" $ do
      runParser (w >>= return) ""   `shouldBe` runParser w ""
      runParser (w >>= return) "12" `shouldBe` runParser w "12"

    it "law: (m >>= f) >>= g  =  m >>= (\\x -> f x >>= g)" $ do
      runParser ((w >>= (return . f)) >>= (return . g)) ""
        `shouldBe` runParser (w >>= (\y -> (return . f) y >>= (return . g))) ""
      runParser ((w >>= (return . f)) >>= (return . g)) "1"
        `shouldBe` runParser (w >>= (\y -> (return . f) y >>= (return . g))) "1"

  describe "Alternative instance for Parser" $ do
    it "law: empty <|> w = w" $ do
      runParser (empty <|> w) ""   `shouldBe` runParser w ""
      runParser (empty <|> w) "12" `shouldBe` runParser w "12"

    it "law: w <|> empty = w" $ do
      runParser (w <|> empty) ""   `shouldBe` runParser w ""
      runParser (w <|> empty) "12" `shouldBe` runParser w "12"

    it "law: u <|> (v <|> w) = (u <|> v) <|> w" $ do
      runParser ((pure 0) <|> ((pure 1) <|> w)) ""   `shouldBe`
                                  runParser (((pure 0) <|> (pure 1)) <|> w) ""
      runParser ((pure 0) <|> ((pure 1) <|> w)) "12" `shouldBe`
                                  runParser (((pure 0) <|> (pure 1)) <|> w) "12"

  describe "Basic combinators" $ do
    it "ok" $ do
      runParser ok ""      `shouldBe` Just ((), [])
      runParser ok [1,2,3] `shouldBe` Just ((), [1,2,3])
      runParser ok "abc"   `shouldBe` Just ((), "abc")

    it "eof" $ do
      runParser eof ""      `shouldBe` Just ((), [])
      runParser eof [1,2,3] `shouldBe` Nothing
      runParser eof "abc"   `shouldBe` Nothing

    it "satisfy" $ do
      runParser (satisfy (== ""))   []     `shouldBe` Nothing
      runParser (satisfy (== 'a'))  "b"    `shouldBe` Nothing
      runParser (satisfy (== 1))    [2]    `shouldBe` Nothing
      runParser (satisfy (== "ab")) ["ab"] `shouldBe` Just ("ab", [])
      runParser (satisfy (== 'a'))  "abc"  `shouldBe` Just ('a', "bc")
      runParser (satisfy (== 1))    [1]    `shouldBe` Just (1, [])

    it "element" $ do
      runParser (element 'a')  "a"       `shouldBe` Just ('a', [])
      runParser (element 'a')  "abc"     `shouldBe` Just ('a', "bc")
      runParser (element 1)    [1, 2, 3] `shouldBe` Just (1, [2, 3])
      runParser (element 'a')  "b"       `shouldBe` Nothing
      runParser (element 1)    [2, 3]    `shouldBe` Nothing

    it "stream" $ do
      runParser (stream "a")       "a"       `shouldBe` Just ("a", [])
      runParser (stream "a")       "abc"     `shouldBe` Just ("a", "bc")
      runParser (stream "ab")      "abc"     `shouldBe` Just ("ab", "c")
      runParser (stream [1])       [1, 2, 3] `shouldBe` Just ([1], [2, 3])
      runParser (stream [1, 2, 3]) [1, 2, 3] `shouldBe` Just ([1, 2, 3], [])
      runParser (stream "a")       "b"       `shouldBe` Nothing
      runParser (stream [1, 2, 3]) [2, 3]    `shouldBe` Nothing

  describe "Simple parsers" $ do
    it "parserBrackets" $ do
      runParser parserBrackets ""         `shouldBe` Just ((), [])
      runParser parserBrackets "()"       `shouldBe` Just ((), [])
      runParser parserBrackets "()()"     `shouldBe` Just ((), [])
      runParser parserBrackets "(()())()" `shouldBe` Just ((), [])
      runParser parserBrackets "("        `shouldBe` Nothing
      runParser parserBrackets ")"        `shouldBe` Nothing
      runParser parserBrackets "(()"      `shouldBe` Nothing
      runParser parserBrackets ")()"      `shouldBe` Nothing
      runParser parserBrackets "()("      `shouldBe` Nothing
      runParser parserBrackets "())"      `shouldBe` Nothing
      runParser parserBrackets "((()())"  `shouldBe` Nothing

    it "parserInt" $ do
      runParser parserInt "1"      `shouldBe` Just (1, [])
      runParser parserInt "1234"   `shouldBe` Just (1234, [])
      runParser parserInt "+10"    `shouldBe` Just (10, [])
      runParser parserInt "-10"    `shouldBe` Just (-10, [])
      runParser parserInt "1one"   `shouldBe` Just (1, "one")
      runParser parserInt "-22abc" `shouldBe` Just (-22, "abc")
      runParser parserInt "1-1"    `shouldBe` Just (1, "-1")
      runParser parserInt ""       `shouldBe` Nothing
      runParser parserInt "+"      `shouldBe` Nothing
      runParser parserInt "-"      `shouldBe` Nothing
      runParser parserInt "one1"   `shouldBe` Nothing
      runParser parserInt "+-"     `shouldBe` Nothing
      runParser parserInt "-+"     `shouldBe` Nothing

  describe "Non-Simple parsers" $ do
    it "listlistParser" $ do
      runParser listlistParser "-1, 1" `shouldBe` Nothing
      runParser listlistParser "1"     `shouldBe` Nothing
      runParser listlistParser "1, 1"  `shouldBe` Just ([[1]], "")
      runParser listlistParser "3, 1, 2, 3, 1, 1"
                                     `shouldBe` Just ([[1, 2, 3], [1]], "")
      runParser listlistParser "2, 1,+10  , 3,5,-7, 2"
                                     `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
      runParser listlistParser "  2    ,1, +10,    3, 5,       -7,    2    "
                                     `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
