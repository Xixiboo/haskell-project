module Data.FImage.Geometry
(
  dist
, distO
  -- * Converting
, fromPolar
, toPolar
)
where

import qualified Data.FImage.Geometry.Point      as Point
import qualified Data.FImage.Geometry.PolarPoint as PolarPoint

-- | Compute the distance between two cartesian points.
dist :: Point.Point -> Point.Point -> Float
dist Point.Point {Point.x = x1, Point.y = y1} Point.Point {Point.x = x2, Point.y = y2} = sqrt $ (x2 - x1)**2 + (y2 - y1)**2

-- | Compute the distance from a given point to the origin.
distO :: Point.Point -> Float
distO Point.Point {Point.x = x, Point.y = y} = sqrt $ x**2 + y**2

-- | Convert a polar point to a cartesian point.
fromPolar :: PolarPoint.PolarPoint -> Point.Point
fromPolar PolarPoint.PolarPoint{ PolarPoint.rho = rho, PolarPoint.theta = theta } = Point.mk x1 x2 where
  x1 = rho*cos theta
  x2 = rho*sin theta


-- | Convert a cartesian point to a polar point.
toPolar :: Point.Point -> PolarPoint.PolarPoint
toPolar Point.Point { Point.x = x, Point.y = y } = PolarPoint.mk rho theta where
  rho = sqrt $ x**2 + y**2
  theta = atan2 y x
