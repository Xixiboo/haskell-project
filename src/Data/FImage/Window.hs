module Data.FImage.Window
(
  -- * type
  Window(..)
  -- * constructing
, mk
)
where

-- | Window type definition.
data Window = Window {getWidth  :: Int , getHeight :: Int } deriving (Show)

-- | Make a window from two integers (width and height).
mk :: Int -> Int -> Window
mk w h
  | w <= 0    = error "zero width window"
  | h <= 0    = error "zero height window"
  | otherwise = Window { getWidth = w, getHeight = h }