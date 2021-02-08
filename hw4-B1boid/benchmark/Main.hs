module Main where

import Task1Bench (geometryBench)
import Task3Bench (hashtableBenchs)

main :: IO ()
main = do
  putStrLn $ "\nTask1\n"
  geometryBench
  putStrLn $ "\nTask3\n"
  hashtableBenchs
