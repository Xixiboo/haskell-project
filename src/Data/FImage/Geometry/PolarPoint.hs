module Data.FImage.Geometry.PolarPoint
(
  -- * type
  Angle
, PolarPoint(..)
  -- * constructing
, mk
)
where

-- | PolarPoint type definition
-- Polar coordinates (rho, theta), where rho is the distance from the origin and theta is the angle between
-- the positive x axis and the ray emanating from the origin and passing through the point.
type Angle = Float
data PolarPoint = PolarPoint {rho :: Float, theta :: Angle} deriving (Show, Eq, Ord)

-- | Make a polar point (rho, theta) from distance rho and angle theta.
mk :: Float -> Angle -> PolarPoint
mk rho theta = PolarPoint {rho = rho, theta = theta}

-- | Make a polar point (rho, theta) from distance rho and angle theta.
mk' :: Float -> Angle -> Maybe PolarPoint
mk' rho theta
  | rho < 0   = Nothing
  | otherwise = Just $ PolarPoint {rho = rho, theta = theta}

