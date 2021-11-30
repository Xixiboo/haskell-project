module Data.FImage.View
(
  -- * Type
  View(..)
  -- * Constructing
, mk
, mk0
  -- * Querying
, xMin
, yMin
, xMax
, yMax
, width
, height
)
where

import qualified Data.FImage.Geometry.Point as Point
-- | View type definition.


-- | View type definition.
data View = View { lowerLeftPoint  :: Point.Point
                 , upperRightPoint :: Point.Point
                 } deriving (Show)

-- | Make an axis-parallel view from the lower-left point and the
-- upper-right point.d
mk :: Point.Point -> Point.Point -> View
mk p1 p2 = View p1 p2

-- | Make an 0center axis-parallel view from two floats (width and height).
mk0 :: Float -> Float -> View
mk0 x y = mk (Point.mk (-x) (-y)) (Point.mk x y)

-- | min x value.
xMin :: View -> Float
xMin = Point.x . lowerLeftPoint

-- | min y value.
yMin :: View -> Float
yMin = Point.y . lowerLeftPoint

-- | yMax x value.
xMax :: View -> Float
xMax = Point.x . upperRightPoint

-- | max y value.
yMax :: View -> Float
yMax = Point.x . upperRightPoint

width :: View -> Float
width view = xMax view - xMin view + 1

height :: View -> Float
height view = yMax view - yMin view + 1
