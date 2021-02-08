{-# LANGUAGE LambdaCase #-}

module Block3
  ( Parser (..)

  , ok
  , eof
  , satisfy
  , element
  , stream
  , parserBrackets
  , parserInt
  , listlistParser
  ) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Char (isDigit, isSpace)
import Control.Monad (replicateM)

-- | parser which supports stream of any data
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f (Parser parser) = Parser $ fmap (first f) . parser where
    first f (a, b) = (f a, b)

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (x, s)
  Parser parserF <*> Parser parser = Parser $ \s -> case parserF s of
    Nothing     -> Nothing
    Just (f, t) -> case parser t of
      Nothing     -> Nothing
      Just (a, r) -> Just (f a, r)

instance Monad (Parser s) where
  Parser a >>= f = Parser $ \s -> a s >>= uncurry (runParser . f)

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

-- | parser which always returns 'Just' and saves input
ok
  :: Parser s ()  -- ^ parser type
ok = pure ()

-- | parser which checks the end of input
eof
  :: Parser s ()  -- ^ parser type
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- | parser which fails if element does not conform predicate
satisfy
  :: (s -> Bool)  -- ^ predicate
  -> Parser s s  -- ^ parser type
satisfy predicate = Parser $ \case
  []       -> Nothing
  (x : xs) -> if predicate x then Just (x, xs) else Nothing

-- | parser which proccesses a single element
element
  :: Eq s  -- ^ constraint for equalation
  => s  -- ^ given element
  -> Parser s s  -- ^ parser type
element = satisfy . (==)

-- | parser which proccesses a list of element
stream
  :: Eq s  -- ^ constraint for equalation
  => [s]  -- ^ list of elements to parse
  -> Parser s [s]  -- ^ parser type
stream = traverse element

-- | parser for bracket sequence
parserBrackets
  :: Parser Char ()  -- ^ parser type
parserBrackets =  s >> eof
  where
    s = many $ stream "(" >> s >> stream ")"

-- | parser for positive and negative integers
parserInt
  :: Parser Char Int  -- ^ parser type
parserInt = read <$> (sign <|> int)
  where
    int = some $ satisfy isDigit
    sign = minus <|> plus
    minus = liftA2 (:) (element '-') int
    plus = element '+' >> int

-- | parser for list of lists of numbers
listlistParser
  :: Parser Char [[Int]]  -- ^ parser type
listlistParser = (liftA2 (:) parseList (many $ comma >> parseList)) <* eof
  where
    parseList = numP parserInt >>= flip replicateM (comma >> numP parserInt)
    numP parser = ws >> parser <* ws
    comma = element ','
    ws = many $ satisfy isSpace
