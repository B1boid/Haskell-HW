module Main
  ( main
  ) where

import Test.Hspec (hspec)
import ParserSpec (spec)
import FileSystemSpec (spec)


main :: IO ()
main =
  hspec $ do
    ParserSpec.spec
    FileSystemSpec.spec
