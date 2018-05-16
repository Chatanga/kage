module Graphics.World (
    HasRenderables(..),
    FireBall(..),
    Aircraft(..),
    DirectionLight(..),
    PointLight(..),
    World(..),
    --
    createWorld,
    createAnotherWorld,
    disposeWorld,
    animateWorld,
) where

import Control.Arrow
import Control.Monad
import Control.Monad.State as S
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V

import Graphics.Rendering.OpenGL
import Linear
import Numeric

import Common.Debug
import Common.Misc

import Graphics.Buffer
import Graphics.Catalog
import Graphics.Color
import Graphics.Font
import Graphics.FunctionalGL as FGL
import Graphics.Geometry
import Graphics.Heightmap
import Graphics.Shader
import Graphics.Texture

import Physics.RigidBody as RB
import Physics.Aircraft

----------------------------------------------------------------------------------------------------

class HasRenderables a where
    instanciate :: a -> ResourceIO [Renderable]

data FireBall = FireBall
    {   fireBallPosition :: !(V3 GLfloat)
    ,   fireBallDirection :: !(V3 GLfloat)
    ,   fireBallColor :: !(Color3 GLfloat)
    ,   fireBallAge :: !Double
    }   deriving (Show)

instance HasRenderables FireBall where
    instanciate (FireBall p d (Color3 cx cy cz) a) = do
        renderable <- acquireResourceWith' "FireBall" createFireBall
        transformation <- liftIO $ newMatrix RowMajor (flattenMatrix (mkTransformation noRotation p)) :: ResourceIO (GLmatrix GLfloat)
        let render' p = do
                setUniform p "transformation" transformation
                setUniform p "emissiveColor" (Color4 cx cy cz 1.0)
                renderableRender renderable p
        return [renderable{ renderableRender = render' }]

instance HasRenderables Aircraft where
    instanciate aircraft = do
        let body = aircraftBody aircraft
            p = RB.position body
            o = RB.orientation body

        let createCuboid :: Color4 GLfloat -> V3 GLfloat -> M44 GLfloat -> ResourceIO Renderable
            createCuboid c (V3 w h d) t = do
                renderable <- acquireResourceWith' "SolidBox(1)" (createSolidBox 1)
                transformation <- liftIO $ newMatrix RowMajor (flattenMatrix (t !*! scaled (V4 w h d 1))) :: ResourceIO (GLmatrix GLfloat)
                let render' p = do
                        setUniform p "transformation" transformation
                        setUniform p "color" c
                        renderableRender renderable p
                return renderable{ renderableRender = render' }

        let f = mkTransformation o p !*! translating (negate (aircraftBarycenter aircraft))
        fuselage <- createCuboid grey (V3 18 3 3) f

        let lw = f !*! translating (V3 3 6 0)
        leftWing <- createCuboid lightGrey (V3 4 10 0.2) lw
        let la = lw !*! translating (V3 (-2) 0 0) !*! rotating (V3 0 1 0) (- aircraftAileronControl aircraft) !*! translating (V3 (-0.5) 0 0)
        leftAileron <- createCuboid turquoise (V3 1 10 0.2) la

        let rw = f !*! translating (V3 3 (-6) 0)
        rightWing <- createCuboid lightGrey (V3 4 10 0.2) rw
        let ra = rw !*! translating (V3 (-2) 0 0) !*! rotating (V3 0 1 0) (aircraftAileronControl aircraft) !*! translating (V3 (-0.5) 0 0)
        rightAileron <- createCuboid turquoise (V3 1 10 0.2) ra

        let lhs = f !*! translating (V3 (-7) 3.5 0)
        leftHorzStabilizer <- createCuboid lightGrey (V3 2 4 0.2) lhs
        let le = lhs !*! translating (V3 (-1) 0 0) !*! rotating (V3 0 1 0) (aircraftElevatorControl aircraft) !*! translating (V3 (-0.5) 0 0)
        leftElevator <- createCuboid turquoise (V3 1 4 0.2) le

        let rhs = f !*! translating (V3 (-7) (-3.5) 0)
        rightHorzStabilizer <- createCuboid lightGrey (V3 2 4 0.2) rhs
        let re = rhs !*! translating (V3 (-1) 0 0) !*! rotating (V3 0 1 0) (aircraftElevatorControl aircraft) !*! translating (V3 (-0.5) 0 0)
        rightElevator <- createCuboid turquoise (V3 1 4 0.2) re

        let rhs = f !*! translating (V3 (-7) 0 3.5)
        vertStabilizer <- createCuboid lightGrey (V3 2 0.2 4) rhs
        let re = rhs !*! translating (V3 (-1) 0 0) !*! rotating (V3 0 0 1) (aircraftRudderControl aircraft) !*! translating (V3 (-0.5) 0 0)
        rudder <- createCuboid turquoise (V3 1 0.2 4) re

        return
            [ fuselage
            , leftWing, leftAileron
            , rightWing, rightAileron
            , leftHorzStabilizer, leftElevator
            , rightHorzStabilizer, rightElevator
            , vertStabilizer, rudder
            ]

data DirectionLight = DirectionLight
    {   directionLightColor :: !(Color3 GLfloat)
    ,   directionLightDirection :: !(V3 GLfloat)
    ,   directionLightAmbientIntensity :: !GLfloat
    }   deriving (Show)

data PointLight = PointLight
    {   pointLightPosition :: !(V3 GLfloat)
    ,   pointLightColor :: !(Color3 GLfloat)
    }   deriving (Show)

data World = World
    {   worldTerrain :: !(Heightmap Float, V3 Float)
    ,   worldObjects :: ![Renderable]
    ,   worldFireBalls :: ![FireBall]
    ,   worldAircrafts :: ![Aircraft]
    ,   worldSun :: !DirectionLight
    ,   worldCameras :: ![(String, Camera)]
    ,   worldElapsedTime :: !Double
    }

type DurationInMs = Double

createWorld :: ResourceIO World
createWorld = do
    heightmap <- liftIO $ loadHeightmap "data/heightmap-257.png"
    let heightmapScale = V3 1 1 0.1
    terrain <- createRenderableTerrain heightmap heightmapScale
    spaceInvader <- createSpaceInvader (V3 10 5 25)
    objects <- sequence
        [ createSkyBox
        , return terrain
        -- , createNormalDisplaying heightmap terrain
        , createGrass heightmap heightmapScale
        , return spaceInvader
        , createSpaceInvader (V3 20 15 15)
        -- , createNormalDisplaying 36 spaceInvader
        -- , createTesselatedPyramid
        , createText (V3 10 0 40) (V3 0 0 0.04) (V3 0 (-0.04) 0) "Kage　-　かげ"
        ]
    let world = World
            (heightmap, heightmapScale)
            objects
            []
            []
            (DirectionLight (Color3 1 0.9 0.8 ^* 0.8) (V3 (-1.86) 0.45 (-0.56)) 0.1)
            [("first-camera", Camera (V3 (-5) 0 (1.8 + 20)) 0 0 (pi/3))]
            0
    return world

createAnotherWorld :: ResourceIO World
createAnotherWorld = do
    let aircraft = mkAircraft
    objects <- sequence
        [ createVectorDisplaying (V3 15 0 0) (Color4 1 0 0 1)
        , createVectorDisplaying (V3 0 15 0) (Color4 0 1 0 1)
        , createVectorDisplaying (V3 0 0 15) (Color4 0 0 1 1)
        , createGrid
        ]
    let world = World
            ((0, 0, V.empty), V3 1 1 1)
            objects
            []
            [aircraft]
            (DirectionLight (Color3 1 0.9 0.8 ^* 0.8) (V3 (-1.86) 0.45 (-0.56)) 0.4)
            [("first-camera", Camera (V3 20 5 15) (pi/3) (-pi) (pi/3))]
            0
    return world

disposeWorld :: World -> ResourceIO ()
disposeWorld world =
    mapM_ renderableDispose (worldObjects world)

animateWorld :: DurationInMs -> World -> ResourceIO World
animateWorld timeDelta world = return world' where

    fireBalls = mapMaybe updateFireBall (worldFireBalls world)
    updateFireBall (FireBall p d c a)
        | a < 10 = Just (FireBall (p + d/2) d c (a + timeDelta))
        | otherwise = Nothing

    aircrafts = map (updateAircraft timeDelta) (worldAircrafts world)

    sun = worldSun world
    sun' = sun
        { directionLightDirection = Linear.rotate
            (axisAngle (V3 0 0 1) (realToFrac timeDelta / 4))
            (directionLightDirection sun)
        }

    elapsedTime = worldElapsedTime world + timeDelta

    world' = world
        { worldFireBalls = fireBalls
        , worldAircrafts = aircrafts
        , worldSun = sun'
        , worldElapsedTime = elapsedTime
        }
