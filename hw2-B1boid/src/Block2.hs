module Block2
  ( ArithmeticError(..)
  , Expr(..)
  , Operation(..)

  , eval
  , moving
  ) where

import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)

-- | binary opeations for arithmetic expression
data Operation
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Eq, Show)

-- | arithmetic expression
data Expr
  = Const Int
  | Op Operation Expr Expr
  deriving (Eq, Show)

-- | error which might be in arithmetic expression
data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Eq, Show)

-- | returns the result of given expression
eval
  :: Expr  -- ^ given expressions
  -> Either ArithmeticError Int  -- ^ result which might be error or not
eval (Const a)          = Right a
eval (Op opetation a b) = eval a >>= \x -> eval b >>= evalOp opetation x

-- | returns the result of given operation
evalOp
  :: Operation  -- ^ given binary opeation
  -> Int  -- ^ first number in binary opeation
  -> Int  -- ^ second number in binary opeation
  -> Either ArithmeticError Int  -- ^ result of given operation
evalOp Add a b = Right (a + b)
evalOp Sub a b = Right (a - b)
evalOp Mul a b = Right (a * b)
evalOp Div a b =
  if b /= 0
  then Right (a `div` b)
  else Left DivisionByZero
evalOp Pow a b =
  if b >= 0
  then Right (a ^ b)
  else Left NegativePow

-- | returns the result of Simple Moving Average algorithm
moving
  :: Int  -- ^ given n in algorithm
  -> [Double]  -- ^ given list
  -> [Double]  -- ^ result list of Simple Moving Average algorithm
moving _ [] = []
moving n (x : xs) = x : evalState (calcAv n xs) startState where
  startState = MovState {cnt=1, queue=([x], []), summ=x}

-- | data for moving algorithm
data MovState = MovState {
  cnt   :: Int,
  queue :: ([Double], [Double]),
  summ  :: Double
}

-- | returns last element and rest queue without it
pop
  :: ([a], [a])  -- ^ given queue
  -> (a, ([a], [a]))  -- ^ last element and new queue without it
pop (l, []) = (x, ([], xs))
  where
    (x : xs) = reverse l
pop (l, y : ys) = (y, (l, ys))

-- | adds new element to queue
calcAndUpdate
  :: Int  -- ^ given n in algorithm
  -> Double -- ^ given element
  -> State MovState Double  -- ^ returning new state
calcAndUpdate n element = do
  curState <- get
  let curNum = cnt curState
  if n == curNum
    then do
      let (popEl, (lPart, rPart)) = pop $ queue curState
      let curSum = summ curState + element - popEl
      put MovState{cnt=curNum, queue=(element : lPart, rPart), summ=curSum}
      return $ curSum / fromIntegral curNum
    else do
      let curSum = summ curState + element
      let incN = curNum + 1
      let (curQueue, _) = queue curState
      put MovState{cnt=incN, queue=(element : curQueue, []), summ=curSum}
      return $ curSum / fromIntegral incN

-- | calculates moving averages
calcAv
  :: Int  -- ^ given n in algorithm
  -> [Double] -- ^ given list of elements
  -> State MovState [Double]  -- ^ returning new state
calcAv _ []       = return []
calcAv n (x : xs) = liftA2 (:) (calcAndUpdate n x) (calcAv n xs)
