module Task2
  ( Nat(..)

  , toInt
  , fromInt
  , natAdd
  , natMul
  , natSub
  , natDiv
  , natMod
  , isEven
  ) where

-- | Data type for natural numbers
data Nat
 = Z
 | S Nat
 deriving (Show)

instance Eq Nat where
  Z     == Z     = True
  (S a) == (S b) = a == b
  _     == _     = False

instance Ord Nat where
  compare Z Z         = EQ
  compare Z _         = LT
  compare _ Z         = GT
  compare (S a) (S b) = compare a b

-- | converts 'Nat' to 'Int'
toInt
  :: Nat  -- ^ 'Nat' to convert
  -> Int  -- ^ 'Int' from 'Nat'
toInt Z     = 0
toInt (S a) = 1 + toInt a

-- | converts 'Int' to 'Nat'
fromInt
  :: Int  -- ^ 'Int' to convert
  -> Nat  -- ^ 'Nat' from 'Int'
fromInt num
  | num > 0 = S $ fromInt (num - 1)
  | otherwise = Z

-- | sum of 'Nat's
natAdd
  :: Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ result of operation
natAdd a Z     = a
natAdd a (S b) = S $ natAdd a b

-- | multiplication of 'Nat's
natMul
  :: Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ result of operation
natMul _ Z     = Z
natMul a (S b) = natAdd a $ natMul a b

-- | subtraction of 'Nat's
natSub
  :: Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ result of operation
natSub Z _         = Z
natSub a Z         = a
natSub (S a) (S b) = natSub a b

-- | division of 'Nat's
natDiv
  :: Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ result of operation
natDiv _ Z = error "Division by zero"
natDiv a b =
  case compare a b of
    EQ -> S Z
    LT -> Z
    GT -> S $ natDiv (natSub a b) b

-- | modulo of 'Nat's
natMod
  :: Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ 'Nat' for op
  -> Nat  -- ^ result of operation
natMod _ Z = error "Division by zero"
natMod a b = natSub a $ natMul b $ natDiv a b

-- | checks is the 'Nat' is even
isEven
  :: Nat  -- ^ 'Nat' for check
  -> Bool  -- ^ is given 'Nat' even
isEven Z     = True
isEven (S a) = not (isEven a)
