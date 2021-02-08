{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task1
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , doubleArea
  , perimeterNaive
  , doubleAreaNaive
  ) where


-- | Data type of Point, which represents point in 2D with two 'Int' coordinates.
data Point = Point
  { x :: !Int
  , y :: !Int
  }

-- | Adding operation for points
plus
  :: Point  -- ^ First given point
  -> Point  -- ^ Second given point
  -> Point  -- ^ Result of operation
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Subtraction operation for points
minus
  :: Point  -- ^ First given point
  -> Point  -- ^ Second given point
  -> Point  -- ^ Result of operation
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Scalar product operation for points
scalarProduct
  :: Point  -- ^ First given point
  -> Point  -- ^ Second given point
  -> Int  -- ^ Result of operation
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Pseudoscalar product operation for points
crossProduct
  :: Point  -- ^ First given point
  -> Point  -- ^ Second given point
  -> Int  -- ^ Result of operation
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Distance between two points
distance
  :: Point  -- ^ First given point
  -> Point  -- ^ Second given point
  -> Double  -- ^ Distance between two points
distance (Point x1 y1) (Point x2 y2) = (sqrt . fromIntegral) (dx * dx + dy * dy)
  where
    dx = x2 - x1
    dy = y2 - y1

-- | Calculates perimeter of given polygon
perimeter
  :: [Point]  -- ^ Points that represents polygon
  -> Double  -- ^ Perimeter of given polygon
perimeter = polygonWalk distance

-- | Calculates double area of polygon
doubleArea
  :: [Point]  -- ^ Points that represents polygon
  -> Int  -- ^ Double area of given polygon
doubleArea = abs . polygonWalk crossProduct

-- | Walk the polygon and apply given function
polygonWalk
  :: forall a . (Num a)  -- ^ Number constraint
  => (Point -> Point -> a)  -- ^ Function that is applied to points
  -> [Point]  -- ^ Given polygon
  -> a  -- ^ Result of polygon walk
polygonWalk _    []                  = 0
polygonWalk func polygon@(point : _) = acc polygon 0
  where
    acc :: [Point] -> a -> a
    acc []                                     _   = 0
    acc [singlePoint]                         !res = func singlePoint point + res
    acc (firstPoint : points@(nextPoint : _)) !res = acc points (func firstPoint nextPoint + res)

-- | Calculates perimeter of given polygon
perimeterNaive
  :: [Point]  -- ^ Points that represents polygon
  -> Double  -- ^ Perimeter of given polygon
perimeterNaive = polygonWalkNaive distance

-- | Calculates double area of polygon
doubleAreaNaive
  :: [Point]  -- ^ Points that represents polygon
  -> Int  -- ^ Double area of given polygon
doubleAreaNaive = abs . polygonWalkNaive crossProduct

-- | Walk the polygon and apply given function
polygonWalkNaive
  :: forall a . (Num a)  -- ^ Number constraint
  => (Point -> Point -> a)  -- ^ Function that is applied to points
  -> [Point]  -- ^ Given polygon
  -> a  -- ^ Result of polygon walk
polygonWalkNaive _    []                  = 0
polygonWalkNaive func polygon@(point : _) = acc polygon 0
  where
    acc :: [Point] -> a -> a
    acc []                                    _   = 0
    acc [singlePoint]                         res = func singlePoint point + res
    acc (firstPoint : points@(nextPoint : _)) res = acc points (func firstPoint nextPoint + res)
