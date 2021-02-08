{-# LANGUAGE DeriveFunctor #-}

module Task8
  ( DiseaseInfo (..)
  , Printable (..)
  , initSimulation
  , nStepSimulation
  ) where

import Control.Comonad (Comonad (..))
import System.Random (StdGen, mkStdGen, random, split)
import Data.List (intercalate)


data ListZipper a = LZ [a] a [a]
  deriving Functor

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a : as) x bs) = LZ as a (x : bs)
listLeft _                  = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b : bs)) = LZ (x : as) b bs
listRight _                  = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) <> [x] <> take n rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

mkZipper :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper getLeft genRight e = LZ (iterateTail getLeft e) e (iterateTail genRight e)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = mkZipper listLeft listRight

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }
  deriving Functor

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right
vertical   = mkZipper up   down

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where horizontals = [left, right]
        verticals   = [up, down]

instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical

-- | Data type for a status of disease
data Status
  = Healthy
  | Incubation
  | Ill
  | Immune
  deriving Eq

instance Show Status where
  show Healthy    = "_"
  show Incubation = "$"
  show Ill        = "#"
  show Immune     = "@"

-- | Data type for a information about disease
data DiseaseInfo = DiseaseInfo
  { probability    :: Double  -- ^ Probability to ill from one neighbour
  , incubationDays :: Int  -- ^ Days for incubation period
  , illDays        :: Int  -- ^ Days for ill period
  , immuneDays     :: Int  -- ^ Days for immune period
  }

-- | Data type for a person
data Person = Person
  { status :: Status  -- ^ Status of the person
  , days   :: Int  -- ^ Days of the current status
  , rand   :: StdGen  -- ^ Random generator
  }

instance Show Person where
  show person = show $ status person

-- | Data type for print grid
data Printable a = Printable
  { grid   :: Grid a   -- ^ Current grid
  , radius :: Int  -- ^ Radius of the grid
  }

instance Show a => Show (Printable a) where
  show (Printable grid dist) = do
    let (Grid z) = fmap show grid
    "|" <> intercalate "|" (intercalate ["\n"]
      (map (\lz -> (toList lz dist)) (toList z dist))) <> "|"

-- | Start random seed for generation
seed :: Int
seed = 123

-- | Healthy person
normalPerson :: Person
normalPerson = Person Healthy 0 (mkStdGen seed)

-- | Left and right splits
leftSplit, rightSplit
  :: Person  -- ^ Given person
  -> Person  -- ^ Person with updated generator
leftSplit  c@Person {rand = r} = c {rand = fst $ split r}
rightSplit c@Person {rand = r} = c {rand = snd $ split r}

-- | One person eats bat
eatBat
  :: Grid Person  -- ^ Current grid
  -> Grid Person  -- ^ Grid with one ill person
eatBat g = gridWrite (gridRead g){status = Incubation} g

-- | Generate healthy society
healthySociety :: Grid Person
healthySociety = Grid $ mkZipper
  ((fmap rightSplit . listLeft) . listRight)
  ((fmap leftSplit . listRight) . listLeft)
  (mkZipper leftSplit rightSplit normalPerson)

-- | Initialize a simulation
initSimulation :: Grid Person
initSimulation = eatBat $ healthySociety

-- | Update status to the given person
updateStatus
  :: Person  -- ^ Given person
  -> Status  -- ^ New status of the person
  -> Int  -- ^ Amount od days of current status
  -> Person  -- ^ Person with new status
updateStatus curPerson@Person {status = curStatus, days = curDays, rand = curGen} nextStatus n =
  if curDays + 1 == n
    then Person {status = nextStatus, days = 0, rand = curGen}
    else Person {status = curStatus, days = curDays + 1, rand = curGen}

-- | Rule from disease info
rule
  :: DiseaseInfo  -- ^ Information about disease
  -> Grid Person  -- ^ Currrent grid
  -> Person  -- ^ Person with new status
rule info g = do
  let curPerson@Person {status = curStatus, days = curDays, rand = curGen} = extract g
  case curStatus of
    Healthy -> do
      let (curStrength, newGen) = random curGen
      let neightboursStatuses = map (\direction -> status $ extract $ direction g) neighbours
      let illNeighbors = (length . filter (\s -> s == Ill || s == Incubation)) neightboursStatuses
      let curP = 1 - (1 - probability info) ^ illNeighbors
      if curP > curStrength
        then Person {status = Incubation, days = 0, rand = newGen}
        else Person {status = Healthy, days = 0, rand = newGen}
    Incubation -> updateStatus curPerson Ill     (incubationDays info)
    Ill        -> updateStatus curPerson Immune  (illDays info)
    Immune     -> updateStatus curPerson Healthy (immuneDays info)

-- | Perform one step of the simulation
oneStepSimulation
  :: DiseaseInfo  -- ^ Information about disease
  -> Grid Person  -- ^ Currrent grid
  -> Grid Person  -- ^ Returns new grid
oneStepSimulation info = extend $ rule info

-- | Perform n steps of the simulation
nStepSimulation
  :: Int  -- ^ Amount of steps
  -> DiseaseInfo  -- ^ Information about disease
  -> Grid Person  -- ^ Currrent grid
  -> Grid Person  -- ^ Returns new grid
nStepSimulation 0     _    grid = grid
nStepSimulation steps info grid =
  nStepSimulation (steps - 1) info (oneStepSimulation info grid)
