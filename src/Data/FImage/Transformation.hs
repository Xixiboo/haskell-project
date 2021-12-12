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
import qualified Data.FImage.Image           as Image
import qualified Data.FImage.Geometry.Point  as Point
import qualified Data.FImage.Geometry.Vector as Vector

type Transformation = Image.Image -> Image.Image

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
translate v i Point.Point {Point.x = x1, Point.y = y1} = i p where
  p = Point.mk (x1-Vector.x v) (y1 - Vector.y v)

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
hTranslate float = translate (Vector.mk float 0)

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
vTranslate float = translate (Vector.mk 0 float)

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
scale (Vector.Vector {Vector.x = vx1, Vector.y = vy1}) i (Point.Point { Point.x = x1, Point.y = y1}) =
  i p where
    p = Point.mk (x1/vx1) (y1 / vy1)

{- | Scale a Boolean image horizontally by a given factor. -}
hScale :: Float -> Transformation
hScale float = scale (Vector.mk float 0)

{- | Scale a Boolean image vertically by a given factor. -}
vScale :: Float -> Transformation
vScale float = scale (Vector.mk 0 float)

{- | Scale a Boolean image uniformly by a given factor. -}
uScale :: Float -> Transformation
uScale float = scale (Vector.mk float float)

{- | Rotate a Boolean image uniformly by a given angle. -}
rotate :: Float -> Transformation
rotate angle i Point.Point {Point.x = x1, Point.y = y1} = i p where
  p = Point.mk (x1*cos angle - y1*sin angle) (y1*cos angle + x1*sin angle)
