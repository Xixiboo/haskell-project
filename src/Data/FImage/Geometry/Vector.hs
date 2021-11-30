module Data.FImage.Geometry.Vector
(
  -- * type definition
  Vector(..)
  -- * constructing
, mk
, mkDiag
  -- * transforming
, revX
, revY
, revXY
, invX
, invY
, invXY
)
where

-- | Vector type definition
data Vector = Vector {x:: Float, y :: Float} deriving (Eq, Ord)

-- | Make a vector (x, y) from two floats `x`and `y`.
mk :: Float -> Float -> Vector
mk x y = Vector {x = x, y = y}

-- | Make a vector '(dz, dz)'' from a float 'dz'.
mkDiag :: Float -> Vector
mkDiag z = mk z z

-- | Make the vector (-x, y) from vector (x, y).
revX :: Vector -> Vector
revX Vector {x  = x, y = y} = mk (-x) y

-- | Make the vector (x, -y) from vector (x, y).
revY :: Vector -> Vector
revY Vector {x  = x, y = y} = mk x (-y)

-- | Make the vector (-x, -y) from vector (x, y).
revXY :: Vector -> Vector
revXY Vector {x  = x, y = y} = mk (-x) (-y)

-- | Make the vector (1/x, y) from vector (x, y).
invX :: Vector -> Vector
invX Vector {x  = x, y = y} = mk (1 / x) y

-- | Make the vector (x, 1/y) from vector (x, y).
invY :: Vector -> Vector
invY Vector {x  = x, y = y} = mk x (1 / y)

-- | Make the vector (1/x, 1/y) from vector (x, y).
invXY :: Vector -> Vector
invXY Vector {x  = x, y = y} = mk (1 / x) (1 / y)
