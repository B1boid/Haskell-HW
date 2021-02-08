{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}

module Task4
  ( Script (..)
  , example
  , runScript
  ) where

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad (liftM2)
import Control.Monad.Extra (ifM)
import Control.Monad.Loops (whileM_)


-- | Class for HalyavaScript variable representaion
class Show a => HSVar a

instance HSVar Int
instance HSVar Bool
instance HSVar String

-- | Tagless final HalyavaScript class
class Script expr where
  type Ref expr :: * -> *

  infixl 1 #
  (#) :: expr a -> expr b -> expr b

  infixr 2 @||
  (@||) :: expr Bool -> expr Bool -> expr Bool

  infixr 3 @&&
  (@&&) :: expr Bool -> expr Bool -> expr Bool

  infix 4 @=
  (@=) :: HSVar a => expr (Ref expr a) -> expr a -> expr ()

  infix 4 @<
  (@<) :: (HSVar a, Ord a) => expr a -> expr a -> expr Bool

  infix 4 @>
  (@>) :: (HSVar a, Ord a) => expr a -> expr a -> expr Bool

  infix 4 @>=
  (@>=) :: (HSVar a, Ord a) => expr a -> expr a -> expr Bool

  infix 4 @<=
  (@<=) :: (HSVar a, Ord a) => expr a -> expr a -> expr Bool

  infix 4 @==
  (@==) :: (HSVar a, Eq a) => expr a -> expr a -> expr Bool

  infix 4 @!=
  (@!=) :: (HSVar a, Eq a) => expr a -> expr a -> expr Bool

  infixl 6 @<>
  (@<>) :: expr String -> expr String -> expr String

  infixl 6 @+
  (@+) :: (HSVar a,  Num a) => expr a -> expr a -> expr a

  infixl 6 @-
  (@-) :: (HSVar a,  Num a) => expr a -> expr a -> expr a

  infixl 7 @*
  (@*) :: (HSVar a,  Num a) => expr a -> expr a -> expr a

  hsWithVar :: (HSVar a, HSVar b) => a -> (expr (Ref expr a) -> expr b) -> expr b
  hsTake    :: expr (Ref expr a) -> expr a
  hsConst   :: (HSVar a) => a -> expr a

  hsWhile  :: expr Bool -> expr () -> expr ()
  hsIf     :: expr Bool -> expr () -> expr () -> expr ()
  hsReturn :: expr (Ref expr a) -> expr a


instance Script (ST s) where
  type Ref (ST s) = STRef s

  ref @= expr = do
    hsRef <- ref
    hsExpr <- expr
    writeSTRef hsRef hsExpr

  (#) = (>>)
  (@&&) = liftM2 (&&)
  (@||) = liftM2 (||)
  (@>)  = liftM2 (>)
  (@<)  = liftM2 (<)
  (@>=) = liftM2 (>=)
  (@<=) = liftM2 (<=)
  (@==) = liftM2 (==)
  (@!=) = liftM2 (/=)
  (@<>) = liftM2 (<>)
  (@+)  = liftM2 (+)
  (@-)  = liftM2 (-)
  (@*)  = liftM2 (*)

  hsWithVar var block = do
    ref <- newSTRef $ var
    block (return ref)

  hsTake hsVar = do
    var <- hsVar
    readSTRef var

  hsConst var = do
    ref <- newSTRef $ var
    readSTRef ref

  hsReturn = hsTake
  hsWhile = whileM_
  hsIf = ifM

-- | Takes HalyavaScript code and runs it (runScript example)
runScript :: (forall s. ST s a) -> a
runScript = runST

-- | Example of HalyavaScript code
example
  :: Script a
  => a String
example =
    hsWithVar "Hello!" $ \greeting ->
    hsWithVar (5 :: Int) $ \rank ->
    hsWithVar False $ \isReady ->
    greeting @= hsTake greeting @<> hsConst " Your rank is " #
    hsWhile (hsTake isReady @== hsConst False)
      (
       rank @= hsTake rank @- hsConst 1 #
       hsIf (hsTake rank @< hsConst 0)
        (
         isReady @= hsConst True
        )
        (
         greeting @= hsTake greeting @<> hsConst "*"
        )
      ) #
    hsReturn greeting
