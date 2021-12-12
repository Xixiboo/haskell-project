module Main where

import Data.Foldable as F
import qualified Data.FImage.BMP             as BMP
import qualified Data.FImage.Geometry.Vector as Vector
import qualified Data.FImage.Image           as Image
import qualified Data.FImage.Image.Simple    as Image.Simple
import qualified Data.FImage.Image.Gallery   as Image.Gallery
import qualified Data.FImage.Transformation  as Transformation
import qualified Data.FImage.View            as View
import qualified Data.FImage.Window          as Window

-- Helper function. Float Transformationation.
-- doTransformationFloat ::  (Float -> Transformation.Transformation) -> Float -> Image.Image
doTransformationFloat :: (Float -> Transformation.Transformation) -> Float -> Image.Image
doTransformationFloat t f = t f Image.Simple.uSquare

-- Helper function. Vector Transformationation.
-- doTransformationVector ::  (Vector.Vector -> Transformation.Transformation) -> Float -> Float -> Image.Image
doTransformationVector ::  (Vector.Vector -> Transformation.Transformation) -> Float -> Float -> Image.Image
doTransformationVector t dx dy = t v Image.Simple.uSquare
  where
    v = Vector.mk dx dy

-- Translate a unit square according to a vector defined by two floats.
-- translateUSquare :: Float -> Float -> Image.Image
translateUSquare :: Float -> Float -> Image.Image
translateUSquare = doTransformationVector Transformation.translate

-- Translate horizontaly a unit square according to a float.
-- hTranslateUSquare :: Float -> Image.Image
hTranslateUSquare :: Float -> Image.Image
hTranslateUSquare = doTransformationFloat Transformation.hTranslate

-- Translate verticaly a unit square according to a float.
-- vTranslateUSquare :: Float -> Image.Image
vTranslateUSquare :: Float -> Image.Image
vTranslateUSquare = doTransformationFloat Transformation.vTranslate

-- -- Scale a unit square according to a vector defined by two floats.
-- scaleUSquare :: Float -> Float -> Image.Image
scaleUSquare :: Float -> Float -> Image.Image
scaleUSquare = doTransformationVector Transformation.scale

-- Scale horizontaly a unit square according to a float.
-- hScaleUSquare :: Float -> Image.Image
hScaleUSquare :: Float -> Image.Image
hScaleUSquare = doTransformationFloat Transformation.hScale

-- Scale verticaly a unit square according to a float.
-- vScaleUSquare :: Float -> Image.Image
vScaleUSquare :: Float -> Image.Image
vScaleUSquare = doTransformationFloat Transformation.vScale

-- Scale uniformaly a unit square according to a float.
-- uScaleUSquare :: Float -> Image.Image
uScaleUSquare :: Float -> Image.Image
uScaleUSquare = doTransformationFloat Transformation.uScale

-- Rotate a unit square according to a float.
-- rotateUSquare :: Float -> Image.Image
rotateUSquare :: Float -> Image.Image
rotateUSquare = doTransformationFloat Transformation.rotate

-- Rotate, translate and scale a unit Square
-- rotateTranslateScaleUSquare :: Float -> Float -> Float -> Float -> Float -> Image.Image
rotateTranslateScaleUSquare :: Float -> Float -> Float -> Float -> Float -> Image.Image
rotateTranslateScaleUSquare t dx dy sx sy = scale . translate . rotate $ i
  where
    scale     = Transformation.scale (Vector.mk sx sy)
    translate = Transformation.translate (Vector.mk dx dy)
    rotate    = Transformation.rotate t
    i         = Image.Simple.uSquare

-- write :: View.View -> Window.Window -> (String, Image.Image) -> IO ()
write :: View.View -> Window.Window -> (String, Image.Image) -> IO ()
write v w (filename, i) = do
  let bmp = BMP.bmp w v i
  BMP.write filename bmp

main :: IO ()
main = do
  let v   = View.mk0 8 8
  let w   = Window.mk 256 256
  F.mapM_ (write v w) [ -- basic boolean images
                        ("hHalfPlane.bmp",   Image.Simple.hHalfPlane 1)
                      , ("hHalfPlane0.bmp",  Image.Simple.hHalfPlane0)
                      , ("vHalfPlane.bmp",   Image.Simple.vHalfPlane 2)
                      , ("vHalfPlane0.bmp",  Image.Simple.vHalfPlane0)
                      , ("hStrip.bmp",       Image.Simple.hStrip 2)
                      , ("uHStrip.bmp",      Image.Simple.uHStrip)
                      , ("vStrip.bmp",       Image.Simple.vStrip 2)
                      , ("uVStrip.bmp",      Image.Simple.uVStrip)
                      , ("cross.bmp",        Image.Simple.cross 2)
                      , ("checker.bmp",      Image.Simple.checker)
                      , ("altRings.bmp",     Image.Simple.altRings)
                      , ("disk.bmp",         Image.Simple.disk 2)
                      , ("square.bmp",       Image.Simple.square 2)
                      , ("uSquare.bmp",      Image.Simple.uSquare)
                      , ("polarChecker.bmp", Image.Simple.polarChecker 7)
                      , ("polarDancer.bmp",  Image.Simple.polarDancer 1)
                      -- Transformationed boolean images
                      , ("translateUSquare.bmp", translateUSquare 2 3)
                      , ("hTranslateUSquare.bmp", hTranslateUSquare 2)
                      , ("vTranslateUSquare.bmp", vTranslateUSquare 3)
                      , ("scaleUSquare.bmp", scaleUSquare 2 3)
                      , ("uScaleUSquare.bmp", uScaleUSquare 3)
                      , ("rotateUSquare.bmp", rotateUSquare (pi / 4))
                      , ("rotateTranslateScaleUSquare.bmp", rotateTranslateScaleUSquare (pi/4) 1 1 2 1)
                      -- Transformation boolean images
                      , ("diamond1.bmp", Image.Gallery.diamond 2 3)
                      , ("diamond2.bmp", Image.Gallery.diamond 3 2)
                      , ("oval1.bmp", Image.Gallery.oval 2 3)
                      , ("oval2.bmp", Image.Gallery.oval 3 2)
                      -- algebra boolean images
                      , ("annulus.bmp", Image.Gallery.annulus 4 2)
                      , ("checkerDisk.bmp", Image.Gallery.checkerDisk 4)
                      , ("checkerSquare.bmp", Image.Gallery.checkerSquare 6)
                      , ("checkerAnnulus.bmp", Image.Gallery.checkerAnnulus 4 1)
                      , ("octogon.bmp", Image.Gallery.octogon 3)
                      , ("xorCircles2.bmp", Image.Gallery.xorCircles2 2 1)
                      , ("checkerXORCircles2.bmp", Image.Gallery.checkerXORCircles2 2 1)
                      , ("xorCircles4.bmp", Image.Gallery.xorCircles4 2 1)
                      , ("xorCircles8.bmp", Image.Gallery.xorCircles8 2 1)
                      , ("eclipse.bmp", Image.Gallery.eclipse 3 2 0.75)
                      , ("lineCircles.bmp", Image.Gallery.lineCircles 2)
                      , ("timeTunnel.bmp", Image.Gallery.timeTunnel)]