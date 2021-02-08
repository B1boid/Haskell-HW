module Task2
  ( parallelIntegration
  , simpleIntegration
  ) where

import Control.Monad.Par.Combinator (InclusiveRange (..), parMapReduceRange)
import Control.Monad.Par (runPar)

-- | Coefficient for more accuracy of integration (the larger it is, the more accurate the result)
k_steps
  :: (Num a)  -- ^ Constraint to use it as Double and as Int
  => a  -- ^ Number
k_steps = 1000

-- | Given function to integration 1/tg(x^2)-cos(x)
func
  :: Int  -- ^ Current x argument
  -> Double  -- ^ Value of function
func x = 1 / tan(((fromIntegral x) / k_steps) ^ 2) - cos((fromIntegral x) / k_steps)

-- | Parallel segment integration function
parallelIntegration
  :: (Int, Int) -- ^ Given line segment
  -> Double  -- ^ Result of integration
parallelIntegration (l, r) = (1 / k_steps) *
  (runPar $ parMapReduceRange (InclusiveRange (k_steps * l) (k_steps * r))
                              (\x -> return $ func x)
                              (\x y -> return $ x + y)
                              0)

-- | Parallel segment integration function
simpleIntegration
  :: (Int, Int) -- ^ Given line segment
  -> Double  -- ^ Result of integration
simpleIntegration (l, r) = (1 / k_steps) * (sum $ map func [k_steps * l .. k_steps * r])
