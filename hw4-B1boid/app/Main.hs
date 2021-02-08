module Main where

import Task8 (DiseaseInfo (..), Printable(..), initSimulation, nStepSimulation)

-- | Default days amount of the sumulation
defaultSumulationDays :: Int
defaultSumulationDays = 20

-- | Default raduis of the grid
defaultGridRadius :: Int
defaultGridRadius = 10

-- | Default information about disease
defaultDiseaseInfo :: DiseaseInfo
defaultDiseaseInfo = DiseaseInfo
  { probability = 0.2
  , incubationDays = 4
  , illDays = 4
  , immuneDays = 4
  }

main :: IO ()
main = do
  let startGrid = initSimulation
  let nthDayGrid = nStepSimulation defaultSumulationDays defaultDiseaseInfo startGrid
  putStrLn $ show $ Printable nthDayGrid defaultGridRadius
