{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Aircraft (
    Aircraft(..),
    mkAircraft,
    updateAircraft
) where

import Data.List
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import Common.Debug as Debug
import Physics.RigidBody

----------------------------------------------------------------------------------------------------

data Wing = Wing
    {   wingPosition :: V3 Float -- in body space (relative to the CG)
    ,   wingOrientation :: Quaternion Float -- in body space
    -- ,   chord :: Float
    -- ,   span :: Float
    ,   control :: Aircraft -> Float
    ,   chordAirfoilLiftCoef :: Float -> Float -- angle of attack -> CL
    ,   spanAirfoilLiftCoef :: Float -> Float -- angle of attack -> CL
    }

mkWing chord span = undefined where
    {-
    l(x) = exp (sin x) - 0.75

    l(-0.25) = 0
    l(0) = 0.25
    l(0.75) = 2
    l(2.75) = 0
    -}
    cl aoa = exp (sin aoa) - 0.75

data Aircraft = Aircraft
    {   aircraftBody :: !(RigidBody Float)
    ,   aircraftBarycenter :: V3 Float
    ,   aircraftWings :: [Wing]
    ,   aircraftThrustControl :: Float
    ,   aircraftAileronControl :: Float
    ,   aircraftElevatorControl :: Float
    ,   aircraftRudderControl :: Float
    }

inverse :: Floating a => Quaternion a -> Quaternion a
inverse = undefined -- https://www.3dgep.com/understanding-quaternions/#Quaternion_Inverse

airDensity = 1.225 -- kg/m3

generateForces :: Wing -> RigidBody Float -> RigidBody Float
generateForces wing body = body' where
    -- The aircraft angular speed is negligible compared to its velocity and hence ignored.
    -- v: velocity in the wing space
    v@(V3 sx sy sz) = inverse (wingOrientation wing) `Linear.rotate` vectorToLocalSpace body (velocity body)
    -- aoa: angle of attack
    aoa = acos (normalize v `dot` unit _z)
    coefLiftX = chordAirfoilLiftCoef wing aoa
    coefLiftY = spanAirfoilLiftCoef wing aoa
    coefLift = (coefLiftX * sx + coefLiftY * sy) / (sx + sy)
    lift = Linear.normalize (v `cross` unit _x) ^* (coefLift * airDensity / norm v^2)
    drag = v ^* (-1)
    body' = foldl (\b f -> addForceAtBodyPoint b f (wingPosition wing)) body [lift, drag]

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
            []
            0 0 0 0
    where
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
        [ \body -> addForce body (V3 0 0 (-gEarth / inverseMass body))
        , integrate (realToFrac durationInMs)
        ]
