module Data.FImage.Geometry.Point
(
  -- * type
  Point(..)
  -- * constructing
, mk
)
where

-- | type definition
data Point = Point {x :: Float, y :: Float} deriving (Show, Eq, Ord)

-- | Make a point '(x, y)'' from two floats 'x' and 'y'.
mk :: Float -> Float -> Point
mk x y = Point {x = x, y = y}
