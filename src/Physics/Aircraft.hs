module Physics.Aircraft (
    Aircraft(..),
    mkAircraft,
    updateAircraft
) where

import Graphics.Rendering.OpenGL
import Linear

import Physics.RigidBody

----------------------------------------------------------------------------------------------------

data Wing = Wing
    {   position :: V3 Float -- in body space (relative to the CG)
    ,   orientation :: Quaternion Float -- in body space
    ,   chord :: Float
    ,   span :: Float
    ,   control :: Aircraft -> Float
    ,   chordAirfoilLiftCoef :: Float -> Float -- angle of attack -> CL
    ,   spanAirfoilLiftCoef :: Float -> Float -- angle of attack -> CL
    }

generateForces :: Wing -> Aircraft -> Aircraft
generateForces = undefined

data Aircraft = Aircraft
    {   aircraftBody :: !(RigidBody Float)
    ,   aircraftBarycenter :: V3 Float
    ,   aircraftThrustControl :: Float
    ,   aircraftAileronControl :: Float
    ,   aircraftElevatorControl :: Float
    ,   aircraftRudderControl :: Float
    }   deriving (Show)

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
    body = integrate (realToFrac durationInMs) (addForce (aircraftBody aircraft) (V3 0 0 (-gEarth)*0))
