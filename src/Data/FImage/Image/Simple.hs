{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.FImage.Image.Simple
(
  -- * Basic boolean images
  hHalfPlane
, hHalfPlane0
, vHalfPlane
, vHalfPlane0
, hStrip
, uHStrip
, vStrip
, uVStrip
, cross
, checker
, altRings
, disk
, uDisk
, square
, uSquare
, polarChecker
, polarDancer
)
where

import qualified Data.FImage.Image               as Image
import qualified Data.FImage.Geometry            as Geometry
import qualified Data.FImage.Geometry.Point      as Point
import qualified Data.FImage.Geometry.PolarPoint as PolarPoint

-- | Horizontal half plane at given y coordinate.
hHalfPlane :: Float -> Image.Image
hHalfPlane y p = Point.y p <= y

-- | Horizontal half plane at y=0 coordinate.
hHalfPlane0 :: Image.Image
hHalfPlane0 = hHalfPlane 0

-- | Vertical half plane at given x coordinate.
vHalfPlane :: Float -> Image.Image
vHalfPlane x p = Point.x p <= x

-- | Horizontal half plane at x=0 coordinate.
vHalfPlane0 :: Image.Image
vHalfPlane0 = vHalfPlane 0

-- | Infinitely horizontal strip of given width.
hStrip :: Float -> Image.Image
hStrip w p = abs (Point.y p) <= w / 2

-- | Infinitely horizontal strip of unit width.
uHStrip :: Image.Image
uHStrip = hStrip 1

-- | Infinitely vertical strip of given width.
vStrip :: Float -> Image.Image
vStrip w p = abs (Point.x p) <= w / 2

-- | Infinitely horizontal strip of unit width.
uVStrip :: Image.Image
uVStrip = vStrip 1

-- | Infinitely horizontal and vertical strips of given width.
cross :: Float -> Image.Image
cross w p = abs (Point.x p) <= w / 2 || abs (Point.y p) <= w / 2

-- | Checker of unit width.
checker :: Image.Image
checker p = even (x' + y')
  where
    x' = fromIntegral . floor $ Point.x p
    y' = fromIntegral . floor $ Point.y p

-- | Concentric of unit width.
altRings :: Image.Image
altRings p = even . fromIntegral . floor $ Geometry.distO p

-- | Disk of given radius.
disk :: Float -> Image.Image
disk r p = Geometry.distO p <= r

-- | Disk of unit radius.
uDisk :: Image.Image
uDisk = disk 1

-- | Square of given length.
square :: Float -> Image.Image
square l p = max x y <= l / 2
  where
    x = abs (Point.x p)
    y = abs (Point.y p)

-- | Square of unit length.
uSquare :: Image.Image
uSquare = square 1.0

-- | Polar checker. The parameter determines the number of alternations, and
-- hence is twice the number of slices.
-- For a cartesian point (x, y), convert to polar point (t', r') and convert
-- back to a cartesian point (r, t * n + pi) and use checker function.
polarChecker :: Float -> Image.Image
polarChecker f = checker . g . Geometry.toPolar
  where
    g p = Point.mk x y
      where
        x = PolarPoint.rho p
        y = f / pi * PolarPoint.theta p

-- | Polar dancer. The parameter determines the “thickness of the dancer's arms”.
polarDancer :: Float -> Image.Image
polarDancer f = vStrip f . Geometry.fromPolar . aux . Geometry.toPolar
  where
    aux p = PolarPoint.mk rho' theta'
      where
        rho' = PolarPoint.theta p
        theta' = PolarPoint.rho p
