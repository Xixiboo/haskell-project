module Data.FImage.Image.Gallery
(
  diamond
, oval
, annulus
, checkerDisk
, checkerSquare
, checkerAnnulus
, octogon
, xorCircles2
, checkerXORCircles2
, xorCircles4
, xorCircles8
, eclipse
, lineCircles
, timeTunnel
)
where

import qualified Data.Foldable as F

import qualified Data.FImage.Image           as Image
import qualified Data.FImage.Image.Simple    as Image.Simple
import qualified Data.FImage.Algebra         as Algebra
import qualified Data.FImage.Transformation  as Transformation
import qualified Data.FImage.Geometry.Vector as Vector

-- translate :: Float -> Float -> Image.Image -> Image.Image
-- translate dx dy = Transformation.translate (Vector.mk dx dy)

scale :: Float -> Float -> Image.Image -> Image.Image
scale dx dy = Transformation.scale (Vector.mk dx dy)

rotate :: Float -> Image.Image -> Image.Image
rotate = Transformation.rotate

diamond :: Float -> Float -> Image.Image
diamond w h = scale (w / sqrt 2) (h / sqrt 2) $ rotate (pi / 4) Image.Simple.uSquare

oval :: Float -> Float -> Image.Image
oval w h = scale (w / sqrt 2) (h / sqrt 2) Image.Simple.uDisk

annulus :: Float -> Float -> Image.Image
annulus f f' = d `Algebra.diff` d'
  where
    d  = Transformation.uScale f  Image.Simple.uDisk
    d' = Transformation.uScale f' Image.Simple.uDisk

checkerDisk :: Float -> Image.Image
checkerDisk f = Image.Simple.disk f `Algebra.inter` Image.Simple.checker

checkerSquare :: Float -> Image.Image
checkerSquare f = Image.Simple.square f `Algebra.inter` Image.Simple.checker

checkerAnnulus :: Float -> Float -> Image.Image
checkerAnnulus f f' = annulus f f' `Algebra.inter` Image.Simple.checker

octogon :: Float -> Image.Image
octogon f = strip0 `Algebra.inter` strip1 `Algebra.inter` strip2 `Algebra.inter` strip3
  where
    strip0 = Image.Simple.hStrip f
    strip1 = Transformation.rotate (pi / 4) strip0
    strip2 = Transformation.rotate (pi / 4) strip1
    strip3 = Transformation.rotate (pi / 4) strip2

xorCircles2 :: Float -> Float -> Image.Image
xorCircles2 f f' = lDisk `Algebra.xor` rDisk
  where
    disk  = Image.Simple.disk f
    lDisk = Transformation.hTranslate (-f') disk
    rDisk = Transformation.hTranslate f'    disk

checkerXORCircles2 :: Float -> Float -> Image.Image
checkerXORCircles2 f f' = Image.Simple.checker `Algebra.xor` xorCircles2 f f'

xorCircles4 :: Float -> Float -> Image.Image
xorCircles4 f f' = i `Algebra.xor` Transformation.rotate (pi / 2) i
  where
    i  = xorCircles2 f f'

xorCircles8 :: Float -> Float -> Image.Image
xorCircles8 f f' = i `Algebra.xor` Transformation.rotate (pi / 4) i
  where
    i  = xorCircles4 f f'

eclipse :: Float -> Float -> Float -> Image.Image
eclipse f f' f'' = d `Algebra.diff` d'
  where
    d  = Image.Simple.disk f
    d' = Transformation.hTranslate f' $ Transformation.uScale f'' d

lineCircles :: Float -> Image.Image
lineCircles f = F.foldr1 Algebra.xor ds
  where
    ds = [mk dx | dx <- [-4,-3..4]]
    mk dx = Transformation.uScale (abs dx) $ Transformation.hTranslate dx (Image.Simple.disk f)

timeTunnel :: Image.Image
timeTunnel = F.foldr1 Algebra.xor hps
  where
    hps = [Transformation.rotate (i * pi / 16) $ Image.Simple.vHalfPlane 0 | i <- [1..16]]
