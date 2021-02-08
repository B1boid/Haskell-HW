module Task1
  ( Day(..)

  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

-- | Data type for days of the week
data Day
 = Mon
 | Tue
 | Wed
 | Thu
 | Fri
 | Sat
 | Sun
 deriving (Show)

instance Enum Day where
  toEnum 0 = Mon
  toEnum 1 = Tue
  toEnum 2 = Wed
  toEnum 3 = Thu
  toEnum 4 = Fri
  toEnum 5 = Sat
  toEnum 6 = Sun
  toEnum num = toEnum $ mod num 7
  fromEnum Mon = 0
  fromEnum Tue = 1
  fromEnum Wed = 2
  fromEnum Thu = 3
  fromEnum Fri = 4
  fromEnum Sat = 5
  fromEnum Sun = 6

instance Eq Day where
  a == b = fromEnum a == fromEnum b

-- | returns the day after given days interval
afterDays
  :: Int  -- ^ number od days after given 'Day'
  -> Day  -- ^ given 'Day'
  -> Day  -- ^ day after daysInterval of given 'Day'
afterDays daysInterval day = toEnum . (+daysInterval) . fromEnum $ day

-- | returns the next day
nextDay
  :: Day  -- ^ given 'Day'
  -> Day  -- ^ next 'Day'
nextDay = afterDays 1

-- | checks if the day is on the weekend
isWeekend
  :: Day  -- ^ given 'Day'
  -> Bool  -- ^ true if given day on the weekend
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- | returns the amount of days to party
daysToParty
  :: Day  -- ^ given 'Day'
  -> Int  -- ^ amount of days before party
daysToParty day = mod (fromEnum Fri - fromEnum day + 7 ) 7
