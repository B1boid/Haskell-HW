module Main
  ( main
  ) where

import Test.Hspec (hspec)

import Block1Spec
import Block2Spec
import Block3Spec


main :: IO ()
main =
  hspec $ do
    Block1Spec.spec
    Block2Spec.spec
    Block3Spec.spec
