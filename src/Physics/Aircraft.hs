{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Aircraft (
    Aircraft(..),
    Wing(..),
    mkWing,
    generateForces,
    mkAircraft,
    updateAircraft
) where

import Data.List
import Linear

import qualified Common.Debug as D
import Physics.RigidBody

----------------------------------------------------------------------------------------------------

data Wing = Wing
    {   wingPosition :: !(V3 Float) -- in body space (relative to the CG)
    ,   wingOrientation :: !(Quaternion Float) -- in body space
    ,   control :: Aircraft -> Float
    ,   chordAirfoilLiftCoef :: Float -> Float -- angle of attack -> CL
    ,   spanAirfoilLiftCoef :: Float -> Float -- angle of attack -> CL
    }

mkWing :: V3 Float -> Quaternion Float -> (Aircraft -> Float) -> Float -> Float -> Wing
mkWing position orientation control chord span = Wing position orientation control chordCoef spanCoef where
    {-
    l(x) = exp (sin x) - 0.75

    l(-0.25) = 0
    l(0) = 0.25
    l(0.75) = 2
    l(2.75) = 0
    -}
    chordCoef aoa = exp (sin aoa) - 0.75
    spanCoef aoa = exp (sin aoa) - 0.75

data Aircraft = Aircraft
    {   aircraftBody :: !(RigidBody Float)
    ,   aircraftBarycenter :: V3 Float
    ,   aircraftWings :: [Wing]
    ,   aircraftThrustControl :: Float
    ,   aircraftAileronControl :: Float
    ,   aircraftElevatorControl :: Float
    ,   aircraftRudderControl :: Float
    }

-- https://www.3dgep.com/understanding-quaternions/#Quaternion_Inverse
inverse :: (Conjugate a, RealFloat a) => Quaternion a -> Quaternion a
inverse = conjugate

airDensity :: Float
airDensity = 1.225 -- kg/m3

generateForces :: Wing -> RigidBody Float -> RigidBody Float
generateForces wing body = body' where
    -- The aircraft angular speed is negligible compared to its velocity and hence ignored.
    -- v: velocity in the wing space
    v@(V3 sx sy _) = inverse (wingOrientation wing) `Linear.rotate` vectorToLocalSpace body (velocity body)
    -- aoa: angle of attack
    aoa = acos (normalize v `dot` unit _z)
    coefLiftX = chordAirfoilLiftCoef wing aoa
    coefLiftY = spanAirfoilLiftCoef wing aoa
    coefLift = (coefLiftX * sx + coefLiftY * sy) / (sx + sy)
    lift = Linear.normalize (v `cross` unit _x) ^* (coefLift * airDensity / norm v^2)
    drag = v ^* (-1)
    body' = foldl (\b f -> addForceAtBodyPoint b f (wingPosition wing)) body [lift, drag]

mkAircraft :: Aircraft
mkAircraft = Aircraft
    (calculateDerivedData $ RigidBody
                (1.0 / mass) -- inverseMass
                (inv33 inertia) -- inverseInertiaTensor
                0.95 0.95
                p o v r
                undefined
                undefined
                []
                [])
            barycenter
            [   mkWing (V3 3 (-6) 1.5) noRot aircraftAileronControl 10 5
            ,   mkWing (V3 3 6 1.5) noRot (negate . aircraftAileronControl) 10 5
            ,   mkWing (V3 (-7.5) (-3.5) 1.5) noRot aircraftElevatorControl 4 3
            ,   mkWing (V3 (-7.5) 3.5 1.5) noRot (negate . aircraftElevatorControl) 4 3
            ,   mkWing (V3 (-7.5) 0 2.5) xRot90 aircraftRudderControl 4 3
            ]
            0 0 0 0
    where
        noRot = axisAngle (unit _x) 0
        xRot90 = axisAngle (unit _x) (pi / 2)
        {- Nieuport 10B (/kg)
        cellule     150
        engine      290
        fuel         85
        payload     165
        TOTAL       690
        -}
        engine = (V3 7.5 0 0, (V3 3 3 3, 290))
        fuselage = (V3 (-1.5) 0 0, (V3 15 3 3, 400))
        mass = 690
        (inertia, barycenter) = mkCompositeCuboidInertiaMatrix [engine, fuselage]
        p = V3 0 0 0
        o = axisAngle (V3 0 0 1) 0
        v = V3 0 0 0
        r = V3 0 0 0

updateAircraft :: Double -> Aircraft -> Aircraft
updateAircraft durationInMs aircraft = aircraft{ aircraftBody = body } where
    body = foldl' (flip id) (aircraftBody aircraft) $
        map generateForces (aircraftWings aircraft) ++
        [ \b -> addForce b (V3 0 0 (-gEarth / inverseMass b))
        , integrate (realToFrac durationInMs)
        ]
