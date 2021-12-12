module Data.FImage.Algebra
(
  universe
, empty
, comp
, inter
, union
, xor
, diff
)
where

import qualified Data.FImage.Image as Image

{- | The all-true boolean image.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> [let p = Point.mk i j in universe p | i <- [0 .. 2], j <- [0 .. 2]]
[True,True,True,True,True,True,True,True,True]
-}
universe :: Image.Image
universe p = True

{- | The all-False boolean image.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> [let p = Point.mk i j in empty p | i <- [0 .. 2], j <- [0 .. 2]]
[False,False,False,False,False,False,False,False,False]
-}
empty :: Image.Image
empty p = False

{- | Complement a boolean image.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> let p = mk 0 0 in comp universe p
False
>>> let p = mk 0 0 in comp empty p
True
>>> f p = abs (Point.x p - Point.y p) >= 1
>>> [let p = Point.mk i j in comp f p | i <- [0 .. 2], j <- [0 .. 2]]
[True,False,False,False,True,False,False,False,True]
-}
comp :: Image.Image -> Image.Image
comp i p = not (i p)

{- | Intersection of two boolean images.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = abs (Point.x p - Point.y p) >= 1
>>> g p = abs (Point.x p - Point.y p) <= 1
>>> [let p = Point.mk i j in inter f g p | i <- [0 .. 2], j <- [0 .. 2]]
[False,True,False,True,False,True,False,True,False]
-}
inter :: Image.Image -> Image.Image -> Image.Image
inter i1 i2 p = i1 p && i2 p

{- | Union of two boolean images.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = abs (Point.x p - Point.y p) >= 1
>>> g p = abs (Point.x p - Point.y p) <= 1
>>> [let p = Point.mk i j in union f g p | i <- [0 .. 2], j <- [0 .. 2]]
[True,True,True,True,True,True,True,True,True]
-}
union :: Image.Image -> Image.Image -> Image.Image
union i1 i2 p = i1 p || i2 p

{- | Xor of two boolean images.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = abs (Point.x p - Point.y p) >= 1
>>> g p = abs (Point.x p - Point.y p) <= 1
>>> [let p = Point.mk i j in xor f g p | i <- [0 .. 2], j <- [0 .. 2]]
[True,False,True,False,True,False,True,False,True]
-}
xor :: Image.Image -> Image.Image -> Image.Image
xor i1 i2 p = i1 p /= i2 p

{- | difference of two boolean images.

>>> import qualified Data.FImage.Geometry.Point as Point
>>> f p = abs (Point.x p - Point.y p) >= 1
>>> g p = abs (Point.x p - Point.y p) <= 1
>>> [let p = Point.mk i j in diff f g p | i <- [0 .. 2], j <- [0 .. 2]]
[False,False,True,False,False,False,True,False,False]
-}
diff :: Image.Image -> Image.Image -> Image.Image
diff i1 i2 p = i1 p && not (i2 p)