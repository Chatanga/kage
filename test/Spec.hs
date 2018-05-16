import Linear

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let rot = pi / 10 :: Double
        axis = normalize (V3 2 (-6) 4) :: V3 Double
        q = axisAngleQ axis rot :: Q Double
        quaternion = axisAngle axis rot :: Quaternion Double
    let rot2 = pi / 3 :: Double
        axis2 = normalize (V3 (-2) 2 4) :: V3 Double
        q2 = axisAngleQ axis2 rot2 :: Q Double
        quaternion2 = axisAngle axis2 rot2 :: Quaternion Double

    {-
    print q
    print quaternion
    print $ fromQ q
    print $ fromQuaternion quaternion
    print $ fromQ q2
    print $ fromQuaternion quaternion2
    print $ q `multiplyQ` q2
    print $ quaternion `multiplyQuaternion` quaternion2

    print $ updateOrientationQ q (V3 1 1 1) 3
    print $ updateOrientationQuaternion quaternion (V3 1 1 1) 3
    -}
    print $ axis + axis2
    print $ axis ^+^ axis2

----------------------------------------------------------------------------------------------------

-- "True" quaternion.
data Q a = Q {
    r :: a,
    i :: a,
    j :: a,
    k :: a
} deriving (Show)

updateOrientationQ :: (Show a, RealFloat a, Epsilon a)
    => Q a  -- ^ orientation (unit quaternion)
    -> V3 a -- ^ rotation (angular velocity)
    -> a    -- ^ time duration
    -> Q a
updateOrientationQ (Q r i j k) rot dt =
    Q   (r + r' * dt)
        (i + i' * dt)
        (j + j' * dt)
        (k + k' * dt)
    where
        Q r' i' j' k' = axisAngleQ' rot `multiplyQ` Q r i j k

-- Attention à la norme
axisAngleQ'
    :: (Show a, RealFloat a)
    => V3 a -- ^ rotation (angular velocity, vector norm = angular speed)
    -> Q a
axisAngleQ' (V3 x y z) = Q 0 (x / 2) (y / 2) (z / 2)

axisAngleQ
    :: (Show a, RealFloat a)
    => V3 a     -- ^ unit axis
    -> a        -- ^ angle
    -> Q a
axisAngleQ (V3 x y z) a = Q c (x * s) (y * s) (z * s) where
    c = cos (a / 2)
    s = sin (a / 2)

multiplyQ :: (Show a, RealFloat a) => Q a -> Q a -> Q a
multiplyQ (Q r i j k) (Q mr mi mj mk) = Q
    (r * mr - i * mi - j * mj - k * mk)
    (r * mi + i * mr + j * mk - k * mj)
    (r * mj + j * mr + k * mi - i * mk)
    (r * mk + k * mr + i * mj - j * mi)

fromQ :: (Show a, RealFloat a) => Q a -> M33 a
fromQ (Q r i j k) = V3
    (V3 (1 - 2*j*j - 2*k*k)     (0 + 2*i*j - 2*k*r)     (0 + 2*i*k + 2*j*r))
    (V3 (0 + 2*i*j + 2*k*r)     (1 - 2*i*i - 2*k*k)     (0 + 2*j*k - 2*i*r))
    (V3 (0 + 2*i*k - 2*j*r)     (0 + 2*j*k + 2*i*r)     (1 - 2*i*i - 2*j*j))

----------------------------------------------------------------------------------------------------

updateOrientationQuaternion :: (Show a, RealFloat a, Epsilon a)
    => Quaternion a -- ^ orientation (unit quaternion)
    -> V3 a         -- ^ rotation (angular velocity)
    -> a            -- ^ time duration
    -> Quaternion a
updateOrientationQuaternion o rot dt = o + axisAngle' rot `multiplyQuaternion` o ^* (dt / 2)

axisAngle' :: (Show a, RealFloat a) => V3 a -> Quaternion a
axisAngle' = Quaternion 0

multiplyQuaternion :: (Show a, RealFloat a) => Quaternion a -> Quaternion a -> Quaternion a
multiplyQuaternion = (*)
