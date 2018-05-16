module Graphics.Heightmap
    ( Heightmap
    , indexToXY
    , xyToIndex
    , getHeightmapValue
    , getInterpolatedHeightmapValue
    ) where

import Data.List
import Data.Function
import Data.Ord
import Data.Tuple
import qualified Data.Vector.Storable as V

import Common.Misc

type Heightmap a = (Int, Int, V.Vector a)

indexToXY :: Heightmap a -> Int -> (Int, Int)
indexToXY (width, _, _) i = swap (divMod i width)

xyToIndex :: Heightmap a -> (Int, Int) -> Int
xyToIndex (width, _, _) (x, y) = y * width + x

getHeightmapValue :: V.Storable a => Heightmap a -> (Int, Int) -> a
getHeightmapValue heightmap@(width, height, values) (x, y) =
    values V.! xyToIndex heightmap (clamp 0 (width - 1) x, clamp 0 (height - 1) y)

-- TODO À tester...
getInterpolatedHeightmapValue :: Heightmap Float -> (Float, Float) -> Float
getInterpolatedHeightmapValue heightmap (x, y) = z where
    (xi, xf) = properFraction x
    (yi, yf) = properFraction y
    squareDistance (dx, dy) = (dx - xi)^2 + (dy - yi)^2
    getHeight (dx, dy) = getHeightmapValue heightmap (xi + dx, yi + dy)
    [z1, z2, z3, z4] = map getHeight [(0, 0), (1, 0), (0, 1), (1, 1)]
    z = if xf + yf < 1
        then (z3 - z1) * yf + (z2 - z1) * xf + z1
        else (z4 - z2) * yf + (z4 - z3) * xf + z4
