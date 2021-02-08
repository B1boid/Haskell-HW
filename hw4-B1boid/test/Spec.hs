module Main
  ( main
  ) where

import Test.Hspec (hspec)
import Task1Spec
import Task4Spec
import Task5Spec
import Task6Spec
import Task7Spec


main :: IO ()
main =
  hspec $ do
    Task1Spec.spec
    Task4Spec.spec
    Task5Spec.spec
    Task6Spec.spec
    Task7Spec.spec
