module Data.FImage.Transformation
(
-- * type
Transformation
-- * Transforming boolean images
, translate
, hTranslate
, vTranslate
, scale
, hScale
, vScale
, uScale
, rotate
)
where

type Transformation = Image -> Image

import qualified Data.FImage.Image           as Image
import qualified Data.FImage.Geometry.Point  as Point
import qualified Data.FImage.Geometry.Vector as Vector
{- | Translate a Boolean image according to a vector.

>>> import qualified Data.FImage.Geometry.Vector as Vector
>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = Point.x p > 0 && Point.y p > 0
>>> v = Vector.mk 1 2
>>> mapM_ print [let p = Point.mk x y in (p, translate v f p) | x <- [0 .. 3], y <- [0 .. 3]]
(Point {x = 0.0, y = 0.0},False)
(Point {x = 0.0, y = 1.0},False)
(Point {x = 0.0, y = 2.0},False)
(Point {x = 0.0, y = 3.0},False)
(Point {x = 1.0, y = 0.0},False)
(Point {x = 1.0, y = 1.0},False)
(Point {x = 1.0, y = 2.0},False)
(Point {x = 1.0, y = 3.0},False)
(Point {x = 2.0, y = 0.0},False)
(Point {x = 2.0, y = 1.0},False)
(Point {x = 2.0, y = 2.0},False)
(Point {x = 2.0, y = 3.0},True)
(Point {x = 3.0, y = 0.0},False)
(Point {x = 3.0, y = 1.0},False)
(Point {x = 3.0, y = 2.0},False)
(Point {x = 3.0, y = 3.0},True)
-}
translate :: Vector.Vector -> Transformation


{- | Translate a Boolean image horizontally by a given distance.

>>> import qualified Data.FImage.Geometry.Vector as Vector
>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = Point.x p > 0 && Point.y p > 0
>>> mapM_ print [let p = Point.mk x y in (p, hTranslate 1 f p) | x <- [0 .. 3], y <- [0 .. 3]]
(Point {x = 0.0, y = 0.0},False)
(Point {x = 0.0, y = 1.0},False)
(Point {x = 0.0, y = 2.0},False)
(Point {x = 0.0, y = 3.0},False)
(Point {x = 1.0, y = 0.0},False)
(Point {x = 1.0, y = 1.0},False)
(Point {x = 1.0, y = 2.0},False)
(Point {x = 1.0, y = 3.0},False)
(Point {x = 2.0, y = 0.0},False)
(Point {x = 2.0, y = 1.0},True)
(Point {x = 2.0, y = 2.0},True)
(Point {x = 2.0, y = 3.0},True)
(Point {x = 3.0, y = 0.0},False)
(Point {x = 3.0, y = 1.0},True)
(Point {x = 3.0, y = 2.0},True)
(Point {x = 3.0, y = 3.0},True)
-}
hTranslate :: Float -> Transformation

{- | Translate a Boolean image vertically by a given distance.

>>> import qualified Data.FImage.Geometry.Vector as Vector
>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = Point.x p > 0 && Point.y p > 0
>>> mapM_ print [let p = Point.mk x y in (p, vTranslate 2 f p) | x <- [0 .. 3], y <- [0 .. 3]]
(Point {x = 0.0, y = 0.0},False)
(Point {x = 0.0, y = 1.0},False)
(Point {x = 0.0, y = 2.0},False)
(Point {x = 0.0, y = 3.0},False)
(Point {x = 1.0, y = 0.0},False)
(Point {x = 1.0, y = 1.0},False)
(Point {x = 1.0, y = 2.0},False)
(Point {x = 1.0, y = 3.0},True)
(Point {x = 2.0, y = 0.0},False)
(Point {x = 2.0, y = 1.0},False)
(Point {x = 2.0, y = 2.0},False)
(Point {x = 2.0, y = 3.0},True)
(Point {x = 3.0, y = 0.0},False)
(Point {x = 3.0, y = 1.0},False)
(Point {x = 3.0, y = 2.0},False)
(Point {x = 3.0, y = 3.0},True)
-}
vTranslate :: Float -> Transformation

{- | Scale a Boolean image according to a vector.

>>> import qualified Data.FImage.Geometry.Vector as Vector
>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = Point.x p >= 0 && Point.x <= 1 && Point.y >= 0 && Point.y <= 1
>>> v = Vector.mk 1 2
>>> mapM_ print [let p = Point.mk x y in (p, scale v f p) | x <- [0 .. 3], y <- [0 .. 3]]
(Point {x = 0.0, y = 0.0},True)
(Point {x = 0.0, y = 1.0},True)
(Point {x = 0.0, y = 2.0},True)
(Point {x = 0.0, y = 3.0},False)
(Point {x = 1.0, y = 0.0},True)
(Point {x = 1.0, y = 1.0},True)
(Point {x = 1.0, y = 2.0},True)
(Point {x = 1.0, y = 3.0},False)
(Point {x = 2.0, y = 0.0},False)
(Point {x = 2.0, y = 1.0},False)
(Point {x = 2.0, y = 2.0},False)
(Point {x = 2.0, y = 3.0},False)
(Point {x = 3.0, y = 0.0},False)
(Point {x = 3.0, y = 1.0},False)
(Point {x = 3.0, y = 2.0},False)
(Point {x = 3.0, y = 3.0},False)
-}
scale :: Vector.Vector -> Transformation

{- | Scale a Boolean image horizontally by a given factor. -}
hScale :: Float -> Transformation

{- | Scale a Boolean image vertically by a given factor. -}
vScale :: Float -> Transformation9

{- | Scale a Boolean image uniformly by a given factor. -}
uScale :: Float -> Transformation

{- | Rotate a Boolean image uniformly by a given angle. -}
rotate :: Float -> Transformation