module Data.FImage.Interval
(
  -- * type
  Interval(..)
  -- * constructing
, mk
  -- * querying
, locate
)
where

data Interval = Interval { getLowerBound :: Float, getUpperBound :: Float }
              deriving (Show)

-- | Make an interval from two floats.
mk :: Float -> Float -> Interval
mk lowerBound upperBound
  | lowerBound > upperBound   = error "null interval"
  | otherwise = Interval { getLowerBound = lowerBound, getUpperBound = upperBound }
  
-- | Return the length of the interval.
len :: Interval -> Float
len i = getUpperBound i - getLowerBound i

-- | Locate in an interval.
locate :: Int -> Int -> Interval -> Float
locate n x i
  | x < 0 || x > n = error "null interval"
  | otherwise      = getLowerBound i +  fromIntegral x * len i / fromIntegral n
