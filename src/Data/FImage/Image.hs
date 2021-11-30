module Data.FImage.Image
(
  -- * type
  Image
)
where

import qualified Data.FImage.Geometry.Point as Point

-- | Image (aka region) type definition
type Image = Point.Point -> Bool
