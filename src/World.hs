module World (
    FireBall(..),
    DirectionLight(..),
    PointLight(..),
    World(..),
    animateWorld,
    --
    createSpaceInvader,
    createFireBall,
    createRenderableTerrain,
    createNormalDisplaying,
    createGrass,
    createRenderableBoxSet,
    createTransparentBox,
    createSkyBox,
    createTesselatedPyramid,
    createScreen,
) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Graphics.Rendering.OpenGL
import Linear
import Numeric

import Buffer
import FunctionalGL
import Geometry
import Heightmap
import Misc
import Shader
import Texture
import Debug

----------------------------------------------------------------------------------------------------

{-
    Inclure à la fois l’aspect physique, sonore et 3D.
    Une sorte de modèle de présentation donc, partagé,
    mais orienté vue.
-}

----------------------------------------------------------------------------------------------------

data FireBall = FireBall
    {   fireBallPosition :: !(V3 GLfloat)
    ,   fireBallDirection :: !(V3 GLfloat)
    ,   fireBallColor :: !(Color3 GLfloat)
    ,   fireBallAge :: !Double
    }   deriving (Show)

data DirectionLight = DirectionLight
    {   directionLightColor :: !(Color3 GLfloat)
    ,   directionLightDirection :: !(V3 GLfloat)
    ,   directionLightAmbientIntensity :: !GLfloat
    }   deriving (Show)

data PointLight = PointLight
    {   pointLightPosition :: !(V3 GLfloat)
    ,   pointLightColor :: !(Color3 GLfloat)
    }   deriving (Show)

data AnyValue = AnyBoolean Bool | AnyString String | AnyInt Int | AnyFloat Float

data Object = Object
    {   objectTransformation :: !(Linear.M44 Float)
    ,   objectBoundingInfo :: !(Maybe BoundingInfo)
    ,   objectOptions :: [(String, AnyValue)] -- [(name, value)]
    -- Isoler dans un (parmi plusieurs) aspect graphique.
    ,   objectRender :: IO () -- Inclut le "program" spécifique mais paramétré de l’extérieur.
    ,   objectDispose :: IO ()
    -- Ajouter un aspect physique.
    }

instance Show Object where
    show _ = "<Object>"

data World = World
    {   worldTerrain :: !(Heightmap Float, V3 Float)
    ,   worldObjects :: ![Object3D] -- TODO as a tree ?
    ,   worldFireBalls :: !(Object3D, [FireBall])
    ,   worldSun :: !DirectionLight
    ,   worldCameras :: ![(String, Camera)]
    ,   worldElapsedTime :: !Double
    }

type DurationInMs = Double

animateWorld :: DurationInMs -> World -> IO World
animateWorld timeDelta world = return world' where

    fireBalls = second (mapMaybe updateFireBall) (worldFireBalls world)
    updateFireBall (FireBall p d c a)
        | a < 10 = Just (FireBall (p + d/2) d c (a + timeDelta))
        | otherwise = Nothing

    sun = worldSun world
    sun' = sun
        { directionLightDirection = Linear.rotate
            (axisAngle (V3 0 0 1) (realToFrac timeDelta / 2))
            (directionLightDirection sun)
        }

    elapsedTime = worldElapsedTime world + timeDelta

    world' = world
        { worldFireBalls = fireBalls
        , worldSun = sun'
        , worldElapsedTime = elapsedTime
        }

----------------------------------------------------------------------------------------------------

alien =
    [ "░░█░░░░░█░░"
    , "░░░█░░░█░░░"
    , "░░███████░░"
    , "░██░███░██░"
    , "███████████"
    , "█░███████░█"
    , "█░█░░░░░█░█"
    , "░░░██░██░░░"
    ]

createSpaceInvader :: IO Object3D
createSpaceInvader = createRenderableBoxSet 2 $ map ((+) (V3 (-1) (-6) 16) . (*) 2) offsets where
    rowCount = fromIntegral (length alien)
    offsets = catMaybes . concat $
        for (zip [0..] alien) $ \(y, row) ->
            for (zip [0..] row) $ \(x, c) ->
                if c == '█' then Just (V3 0 x (rowCount - y)) else Nothing

createFireBall :: IO Object3D
createFireBall = do
    (vao, render, disposeSphere) <- createSphere 0
    (program, disposeProgram) <- createProgramWithShaders
        [ ("fireball_vs.glsl", VertexShader)
        , ("fireball_gs.glsl", GeometryShader)
        , ("fireball_fs.glsl", FragmentShader)
        ]
    return (Object3D program vao render (disposeSphere >> disposeProgram))

createRenderableTerrain :: Heightmap Float -> V3 Float -> IO Object3D
createRenderableTerrain heightmap@(w, h, _) scale = do
    (vao, render, disposeTerrain) <- createTerrain heightmap scale
    (textures, disposeTextures) <- unzip <$> mapMaybeM loadImage
        [ "data/ground_1.png"
        , "data/ground_5.png"
        , "data/ground_3.png"
        ]
    (program, disposeProgram) <- createProgramWithShaders' "terrain_vs.glsl" "terrain_fs.glsl"

    let render' s p =
            withState blend Enabled .
            withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            usingOrderedTextures p textures $ render s p
    let dispose = disposeTerrain >> disposeProgram >> sequence_ disposeTextures

    return (Object3D program vao render' dispose)

createNormalDisplaying :: Heightmap Float -> Object3D -> IO Object3D
createNormalDisplaying (w, h, _) (Object3D _ vao _ _) = do
    let render size program = drawArrays Points 0 (fromIntegral (w * h))
    (program, disposeProgram) <- createProgramWithShaders
        [ ("normal_vs.glsl", VertexShader)
        , ("normal_gs.glsl", GeometryShader)
        , ("normal_fs.glsl", FragmentShader)
        ]
    return (Object3D program vao render doNothing)

createGrass :: Heightmap Float -> V3 Float -> IO Object3D
createGrass heightmap scale = do
    (vao, render, disposeTerrain) <- createRandomMesh heightmap scale
    let configureTexture t = withTexture2D t $ do
            forM_ [S, T] $ \coord -> textureWrapMode Texture2D coord $= (Mirrored, Repeat)
            return t
    (preTextures, disposeTextures) <- unzip <$> mapMaybeM loadImage
        [ "data/grass_02/grass_04/diffus.tga"
        , "data/grass_02/grass_05/diffus.tga"
        , "data/grass_02/grass_06/diffus.tga"
        ]
    textures <- mapM configureTexture preTextures
    (program, disposeProgram) <- createProgramWithShaders
        [ ("grass_vs.glsl", VertexShader)
        , ("grass_gs.glsl", GeometryShader)
        , ("grass_fs.glsl", FragmentShader)
        ]

    let render' s p =
            -- withState blend Enabled .
            -- withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            withState cullFace Nothing .
            usingOrderedTextures p textures $ do
            setUniform p "grassColor" (Color4 1 1 1 1 :: Color4 GLfloat)
            setUniform p "grassAlphaTest" (0.2 :: GLfloat)
            setUniform p "grassAlphaMultiplier" (1.2 :: GLfloat)
            render s p
    let dispose = disposeTerrain >> disposeProgram >> sequence_ disposeTextures

    return (Object3D program vao render' dispose)

createRenderableBoxSet :: Float -> [V3 GLfloat] -> IO Object3D
createRenderableBoxSet edgeSize translations = do
    (vao, render, disposeBox) <- createTexturedBox edgeSize
    Just (texture, disposeTexture) <- loadImage "data/ground_3.png"
    (program, disposeProgram) <- createProgramWithShaders' "enlighted_box_vs.glsl" "enlighted_box_fs.glsl"

    transformations <- mapM
        (newMatrix RowMajor . flattenMatrix . mkTransformation noRotation) translations
        :: IO [GLmatrix GLfloat]
    let render' s p = forM_ transformations $ \transformation -> do
            setUniform p "transformation" transformation
            usingOrderedTextures p [texture] (render s p)

    return (Object3D program vao render' (disposeBox >> disposeProgram >> disposeTexture))

createTransparentBox :: Float -> IO Object3D
createTransparentBox edgeSize = do
    (vao, render, dispose) <- createBox edgeSize
    (program, disposeProgram) <- createProgramWithShaders' "box_vs.glsl" "box_fs.glsl"

    let -- rotation = axisAngle (V3 1 2 3) (pi / 5)
        translation = V3 (-1.5) (-0.5) 0.6
        t = mkTransformation noRotation translation

    transformation <- newMatrix RowMajor (flattenMatrix t) :: IO (GLmatrix GLfloat)

    let render' s p = do
            setUniform p "transformation" transformation
            withState blend Enabled .
                withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
                withState depthMask Disabled .
                withState cullFace Nothing $ render s p

    return (Object3D program vao render' (dispose >> disposeProgram))

createSkyBox :: IO Object3D
createSkyBox = do
    (vao, render, disposeSkyBox) <- createTexturedSkyBox far
    let cloudTextureNames = map
            (\ext -> "data/skybox/cloudtop/cloudtop_" ++ ext ++ ".tga")
            ["dn", "up", "lf", "bk", "rt", "ft"]
        elbrusTextureNames = map
            (\ext -> "data/skybox/elbrus/elbrus_" ++ ext ++ ".jpg")
            ["top", "top", "right", "back", "left", "front"]
        configureTexture t = withTexture2D t $ do
            textureWrapMode Texture2D S $= (Mirrored, Repeat)
            textureWrapMode Texture2D T $= (Mirrored, Repeat)
            return t
    (preTextures, disposeTextures) <- unzip <$> mapMaybeM loadImage cloudTextureNames
    textures <- mapM configureTexture preTextures

    (program, disposeProgram) <- createProgramWithShaders' "skybox_vs.glsl" "skybox_fs.glsl"

    let render' s p =
            withState cullFace Nothing .
            usingOrderedTextures p textures $ render s p
        dispose = disposeSkyBox >> disposeProgram >> sequence_ disposeTextures

    return (Object3D program vao render' dispose)

createTesselatedPyramid :: IO Object3D
createTesselatedPyramid = do
    (vao, render, dispose) <- createPatchPyramid
    (program, disposeProgram) <- createProgramWithShaders
        [ ("triangle_vs.glsl", VertexShader)
        , ("triangle_cs.glsl", TessControlShader)
        , ("triangle_es.glsl", TessEvaluationShader)
        , ("triangle_fs.glsl", FragmentShader)
        ]

    let render' s p = do
            patchVertices $= 3
            withState polygonMode (Line, Line) .
                withState cullFace Nothing $
                render s p

    return (Object3D program vao render' (dispose >> disposeProgram))

createScreen :: IO Object3D
createScreen = do
    (vao, render, dispose) <- createSquare (0, 0) 1.95
    (program, disposeProgram) <- createProgramWithShaders' "screen_vs.glsl" "screen_fs.glsl"
    return (Object3D program vao render (dispose >> disposeProgram))
