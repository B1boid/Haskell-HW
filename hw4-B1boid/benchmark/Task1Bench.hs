module Task1Bench
  ( geometryBench
  ) where

import Task1 (Point (..), perimeter, doubleArea, perimeterNaive, doubleAreaNaive)

import Criterion.Main (bench, bgroup, defaultMain, nf)

-- | Function that generates points, just for perfomance test
generatePoints :: Int -> [Point]
generatePoints n = fmap (\pnt -> Point pnt pnt) [1 .. n]

-- | Bench for double area and perimetr
geometryBench :: IO()
geometryBench = defaultMain [
    bgroup "perimeter" [
      bench "Fast method 10^7" $ nf perimeter points,
      bench "Naive method 10^7" $ nf perimeterNaive points
    ],
    bgroup "double area" [
      bench "Fast method 10^7" $ nf doubleArea points,
      bench "Naive method 10^7" $ nf doubleAreaNaive points
    ]
  ] where
      points = generatePoints 10000000
