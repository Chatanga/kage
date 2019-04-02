module Graphics.Geometry (
    BoundingInfo(..),
    Camera(..),
    near,
    far,
    getSight,
    getBearing,
    getLeft,
    getUp,
    toWorld,
    --
    noRotation,
    noTranslation,
    scaling,
    translating,
    rotating,
    --
    showV1,
    showV2,
    showV3,
    showV4,
    toVector3,
    toColor3,
    toColor4
) where

import Data.List

import Graphics.Rendering.OpenGL
import Linear
import Numeric

----------------------------------------------------------------------------------------------------

data BoundingInfo
    = BoundingSphere (Linear.V3 Float) Float -- center radius
    | OrthoBoundingBox (Linear.V3 Float) (Linear.V3 Float) -- center (witdh, height, depth)

-- See https://en.wikipedia.org/wiki/Horizontal_coordinate_system
data Camera = Camera
    {   cameraPosition :: !(V3 GLfloat)
    ,   cameraAltitude :: !GLfloat
    ,   cameraAzimuth :: !GLfloat
    ,   cameraFov :: !GLfloat
    }   deriving (Show)

near = 1 :: Float
far = 1000 :: Float

getSight :: Camera -> V3 GLfloat
getSight camera = Linear.rotate (axisAngle (getLeft camera) (cameraAltitude camera)) (getBearing camera)

getBearing :: Camera -> V3 GLfloat
getBearing camera = V3 (cos a) (sin a) 0 where a = cameraAzimuth camera

getLeft :: Camera -> V3 GLfloat
getLeft camera = V3 (cos a) (sin a) 0 where a = cameraAzimuth camera + pi / 2

getUp :: Camera -> V3 GLfloat
getUp camera = V3 0 0 1

toWorld :: Size -> V2 Double -> M44 GLfloat -> M44 GLfloat -> V4 GLfloat
toWorld (Size width height) (V2 x y) projection camera = rectify target where
    (w, h) = (realToFrac width, realToFrac height)
    viewport = V4
        (V4 (w/2) 0 0 (w/2))
        (V4 0 (-h/2) 0 (h/2))
        (V4 0 0 (far - near) near)
        (V4 0 0 0 1)
    t = viewport !*! projection !*! camera
    target = inv44 t !* V4 (realToFrac x) (realToFrac y) 0 1
    rectify (V4 x y z w) = V4 (x / w) (y / w) (z / w) 1

----------------------------------------------------------------------------------------------------

noRotation :: (Epsilon a, Floating a) => Quaternion a
noRotation = axisAngle (V3 0 1 0) 0

noTranslation :: Floating a => V3 a
noTranslation = V3 0 0 0

scaling :: Floating a => V3 a -> M44 a
scaling (V3 sx sy sz) = V4
    (V4 sx 0 0 0)
    (V4 0 sy 0 0)
    (V4 0 0 sz 0)
    (V4 0 0 0 1)

translating :: Floating a => V3 a -> M44 a
translating (V3 dx dy dz) = V4
    (V4 1 0 0 dx)
    (V4 0 1 0 dy)
    (V4 0 0 1 dz)
    (V4 0 0 0 1)

rotating :: (Epsilon a, Floating a) => V3 a -> a -> M44 a
rotating axis angle = mkTransformation (axisAngle axis angle) noTranslation

showV1 x = showGFloat (Just 2) (realToFrac x) ""
showV2 (V2 x y) = showVn [x, y]
showV3 (V3 x y z) = showVn [x, y, z]
showV4 (V4 x y z w) = showVn [x, y, z, w]

showVn v = "(" ++ intercalate ", " (map showV1 v) ++ ")"

toVector3 :: V3 a -> Vector3 a
toVector3 (V3 x y z) = Vector3 x y z

toColor3 :: V3 a -> Color3 a
toColor3 (V3 r g b) = Color3 r g b

toColor4 :: V4 a -> Color4 a
toColor4 (V4 r g b a) = Color4 r g b a
