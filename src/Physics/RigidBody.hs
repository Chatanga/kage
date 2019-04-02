{-# LANGUAGE DuplicateRecordFields #-}

module Physics.RigidBody (
    gEarth,
    mkCuboidInertiaMatrix,
    mkCompositeCuboidInertiaMatrix,
    RigidBody(..),
    affineInverse,
    positionToLocalSpace,
    positionToWorldSpace,
    vectorToLocalSpace,
    vectorToWorldSpace,
    calculateDerivedData,
    integrate,
    addForce,
    addForceAtPoint,
    addForceAtBodyPoint,
    addTorque
) where

import Control.Applicative

import Linear
import Control.Lens

import Common.Debug as D

----------------------------------------------------------------------------------------------------

gEarth :: Float
gEarth = 9.80665 -- m/s2

-- Solid cuboid
mkCuboidInertiaMatrix :: (Show a, Floating a)
     => V3 a -- ^ (width, height, depth)
     -> a -- ^ mass
     -> M33 a
mkCuboidInertiaMatrix (V3 w h d) m = scaled $ V3
    (m * (h^2 + d^2) / 12)
    (m * (w^2 + d^2) / 12)
    (m * (w^2 + h^2) / 12)

-- https://en.wikipedia.org/wiki/Parallel_axis_theorem
mkCompositeCuboidInertiaMatrix
    :: (Show a, Floating a)
    => [(V3 a, (V3 a, a))] -- [(centerOfGravity, (geometry, mass)]
    -> (M33 a, V3 a)
mkCompositeCuboidInertiaMatrix cuboids = (scaled compositeMoment, barycenter) where
    compositeMass = sum (map (snd . snd) cuboids)
    barycenter@(V3 bx by bz) = foldl (^+^) (V3 0 0 0) (map (\(p, (_, m)) -> p ^* m) cuboids) ^/ compositeMass
    getDisplacedMoment (V3 x y z, (V3 w h d, m)) = V3
        ((m * (h^2 + d^2) / 12) + m * (x - bx)^2)
        ((m * (w^2 + d^2) / 12) + m * (y - by)^2)
        ((m * (w^2 + h^2) / 12) + m * (z - bz)^2)
    compositeMoment = foldl (^+^) (V3 0 0 0) (map getDisplacedMoment cuboids)

----------------------------------------------------------------------------------------------------

data RigidBody a = RigidBody {

-- Characteristic Data and State

    {-
    Holds the inverse of the mass of the rigid body. It
    is more useful to hold the inverse mass because
    integration is simpler, and because in real time
    simulation it is more useful to have bodies with
    infinite mass (immovable) than zero mass
    (completely unstable in numerical simulation).
    -}
    inverseMass :: !a,

    {-
    Holds the inverse of the body's inertia tensor. The
    inertia tensor provided must not be degenerate
    (that would mean the body had zero inertia for
    spinning along one axis). As long as the tensor is
    finite, it will be invertible. The inverse tensor
    is used for similar reasons to the use of inverse
    mass.

    The inertia tensor, unlike the other variables that
    define a rigid body, is given in body space.
    -}
    inverseInertiaTensor :: !(M33 a),

    {-
    Holds the amount of damping applied to linear
    motion.  Damping is required to remove energy added
    through numerical instability in the integrator.
    -}
    linearDamping :: !a,

    {-
    Holds the amount of damping applied to angular
    motion.  Damping is required to remove energy added
    through numerical instability in the integrator.
    -}
    angularDamping :: !a,

    {-
    Holds the linear position of the rigid body in
    world space.
    -}
    position :: !(V3 a),

    {-
    Holds the angular orientation of the rigid body in
    world space.
    -}
    orientation :: !(Quaternion a),

    {-
    Holds the linear velocity of the rigid body in
    world space.
    -}
    velocity :: !(V3 a),

    {-
    Holds the angular velocity, or rotation, of the
    rigid body in world space.
    -}
    rotation :: !(V3 a),

-- Derived Data

    {-
    Holds the inverse inertia tensor of the body in world space.
    The inverse inertia tensor member is specified in the body's local space.
    -}
    inverseInertiaTensorWorld :: M33 a,

    {-
    Holds a transform matrix for converting body space into world space and vice versa.
    -}
    transformMatrix :: M43 a,

-- Force and Torque Accumulators

    {-
    Holds the accumulated force to be applied at the next
    integration step.
    -}
    forcesAccum :: ![V3 a],

    {-
    Holds the accumulated torque to be applied at the next
    integration step.
    -}
    torquesAccum :: ![V3 a]
} deriving (Show)

affineInverse :: (Show a, RealFloat a) => M43 a -> M43 a
affineInverse m = mkTransformationMat (transpose o) (negated t) ^. _m43
    where
        o = m ^. _m33
        t = transpose m ^. translation -- TODO Pas net...

positionToLocalSpace :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a
    -> V3 a
positionToLocalSpace body p = (m43_to_m44 (affineInverse (transformMatrix body)) !* point p) ^. _xyz

positionToWorldSpace :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a
    -> V3 a
positionToWorldSpace body p = (m43_to_m44 (transformMatrix body) !* point p) ^. _xyz

vectorToLocalSpace :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a
    -> V3 a
vectorToLocalSpace body p = (m43_to_m44 (affineInverse (transformMatrix body)) !* vector p) ^. _xyz

vectorToWorldSpace :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a
    -> V3 a
vectorToWorldSpace body p = (m43_to_m44 (transformMatrix body) !* vector p) ^. _xyz

----------------------------------------------------------------------------------------------------

{-
Calculates internal data from state data. This should be called
after the body's state is altered directly (it is called
automatically during integration). If you change the body's state
and then intend to integrate before querying any data (such as
the transform matrix), then you can ommit this step.
-}
calculateDerivedData :: (Show a, RealFloat a) =>  RigidBody a -> RigidBody a
calculateDerivedData body = body { transformMatrix = tm, inverseInertiaTensorWorld = iitw } where
    tm = mkTransformationMat (transpose o) (negated p) ^. _m43 where
            o = fromQuaternion (orientation body)
            p = position body
    iitw = (tm ^. _m33) !*! inverseInertiaTensor body

{-
Integrates the rigid body forward in time by the given amount.
This function uses a Newton-Euler integration method, which is a
linear approximation to the correct integral. For this reason it
may be inaccurate in some cases.
-}
integrate :: (Show a, RealFloat a, Epsilon a) => a -> RigidBody a -> RigidBody a
integrate duration body = body'' where

    p = position body
    o = orientation body
    v = velocity body
    r = rotation body

    -- Force and Torque Accumulators
    f = foldl (+) (V3 0 0 0) (forcesAccum body)
    t = foldl (+) (V3 0 0 0) (torquesAccum body)

    linearAcceleration = inverseMass body *^ f
    v' = (v ^+^ (linearAcceleration ^* duration)) ^* (linearDamping body ** duration)
    p' = p ^+^ (v' ^* duration)

    angularAcceleration = inverseInertiaTensorWorld body !* t
    r' = (r ^+^ (angularAcceleration ^* duration)) ^* (angularDamping body ** duration)
    o' = o + Quaternion 0 r' * o ^* (duration * 0.5)

    body' = body{ position = p', velocity = v', orientation = o', rotation = r', forcesAccum = [], torquesAccum = [] }
    body'' = calculateDerivedData body'

{-
Adds the given force to centre of mass of the rigid body.
The force is expressed in world-coordinates.
-}
addForce :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a -- ^ The force to apply.
    -> RigidBody a
addForce body force = body{ forcesAccum = force : forcesAccum body }

{-
Adds the given force to the given point on the rigid body.
The direction of the force is given in world coordinates,
but the application point is given in body space.
This is useful for spring forces, or other forces fixed to the body.
-}
addForceAtBodyPoint :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a -- ^ The force to apply.
    -> V3 a -- ^ The location at which to apply the force, in body-coordinates.
    -> RigidBody a
addForceAtBodyPoint body force localPosition = addForceAtPoint body force position
    where position = positionToWorldSpace body force

{-
Adds the given force to the given point on the rigid body.
Both the force and the application point are given in world space.
Because the force is not applied at the centre of mass,
it may be split into both a force and torque.
-}
addForceAtPoint :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a -- ^ The force to apply.
    -> V3 a -- ^ The location at which to apply the force, in world-coordinates.
    -> RigidBody a
addForceAtPoint body force p =
    body{   forcesAccum = force : forcesAccum body
        ,   torquesAccum = torque : torquesAccum body
        }
    -- Convert to coordinates relative to center of mass.
    where
        localPosition = p ^-^ position body
        torque = localPosition `cross` force

{-
Adds the given torque to the rigid body.
The force is expressed in world-coordinates.
-}
addTorque :: (Show a, RealFloat a)
    => RigidBody a
    -> V3 a -- ^ The torque to apply.
    -> RigidBody a
addTorque body torque = body{ torquesAccum = torque : torquesAccum body }
