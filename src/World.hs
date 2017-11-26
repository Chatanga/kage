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
    createHeightmapNormalDisplaying,
    createNormalDisplaying,
    createGrass,
    createRenderableBoxSet,
    createTransparentBox,
    createSkyBox,
    createTesselatedPyramid,
    createScreen,
    createDeferredScreen
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
    ,   objectRender :: Program -> IO ()
    ,   objectDispose :: IO ()
    -- Ajouter un aspect physique.
    }

instance Show Object where
    show _ = "<Object>"

data World = World
    {   worldTerrain :: !(Heightmap Float, V3 Float)
    ,   worldObjects :: ![Renderable] -- TODO as a tree ?
    ,   worldFireBalls :: !(Renderable, [FireBall])
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
            (axisAngle (V3 0 0 1) (realToFrac timeDelta / 4))
            (directionLightDirection sun)
        }

    elapsedTime = worldElapsedTime world + timeDelta

    world' = world
        { worldFireBalls = fireBalls
        , worldSun = sun
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

createSpaceInvader :: V3 Float -> IO Renderable
createSpaceInvader position = createRenderableBoxSet 2 $ map ((+) position . (*) 2) offsets where
    rowCount = fromIntegral (length alien)
    offsets = catMaybes . concat $
        for (zip [0..] alien) $ \(y, row) ->
            for (zip [0..] row) $ \(x, c) ->
                if c == '█' then Just (V3 0 x (rowCount - y)) else Nothing

createFireBall :: IO Renderable
createFireBall = do
    (vao, render, disposeSphere) <- createSphere 0
    (program, disposeProgram) <- createProgramWithShaders
        [ ("fireball_vs.glsl", VertexShader)
        , ("fireball_gs.glsl", GeometryShader)
        , ("fireball_fs.glsl", FragmentShader)
        ]
    let render' p = withState blend Enabled .
            withState depthMask Disabled .
            withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            withState cullFace (Just Back) $ render p
    return (Renderable [(ForwardShadingStage, program)] vao render' (disposeSphere >> disposeProgram))

createRenderableTerrain :: Heightmap Float -> V3 Float -> IO Renderable
createRenderableTerrain heightmap scale = do
    (vao, render, disposeTerrain) <- createTerrain heightmap scale
    (textures, disposeTextures) <- unzip <$> mapMaybeM loadImage
        [ "data/ground_1.png"
        , "data/ground_5.png"
        , "data/ground_3.png"
        ]
    (program1, disposeProgram1) <- createProgramWithShaders' "shadow_vs.glsl" "shadow_fs.glsl"
    (program2, disposeProgram2) <- createProgramWithShaders' "generic_vs.glsl" "terrain_fs.glsl"
    (program3, disposeProgram3) <- createProgramWithShaders' "generic_vs.glsl" "terrain_deferred_fs.glsl"

    let programs =
            [ (ShadowMappingStage, program1)
            , (ForwardShadingStage, program2)
            , (DeferredShadingStage, program3)
            ]
        disposeAll = do
            disposeTerrain
            disposeProgram1
            disposeProgram2
            disposeProgram3
            sequence_ disposeTextures

    let render' p =
            {- Ne sert à rien et pose problème en rendu différé.
            withState blend Enabled
            withState blendFunc (SrcAlpha, OneMinusSrcAlpha)
            -}
            usingOrderedTextures p textures $ render p

    return (Renderable programs vao render' disposeAll)

createHeightmapNormalDisplaying :: Heightmap Float -> Renderable -> IO Renderable
createHeightmapNormalDisplaying (w, h, _) = createNormalDisplaying (fromIntegral (w * h))

createNormalDisplaying :: Int -> Renderable -> IO Renderable
createNormalDisplaying pointCount (Renderable _ vao _ _) = do
    let render p = withBinding bindVertexArrayObject vao $ drawArrays Points 0 (fromIntegral pointCount)
    (program, disposeProgram) <- createProgramWithShaders
        [ ("normal_vs.glsl", VertexShader)
        , ("normal_gs.glsl", GeometryShader)
        , ("normal_fs.glsl", FragmentShader)
        ]
    return (Renderable [(ForwardShadingStage, program)] vao render doNothing)

createGrass :: Heightmap Float -> V3 Float -> IO Renderable
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

    let render' p =
            -- withState blend Enabled .
            -- withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
            withState cullFace Nothing .
            usingOrderedTextures p textures $ do
            setUniform p "grassColor" (Color4 1 1 1 1 :: Color4 GLfloat)
            setUniform p "grassAlphaTest" (0.2 :: GLfloat)
            setUniform p "grassAlphaMultiplier" (1.2 :: GLfloat)
            render p
    let dispose = disposeTerrain >> disposeProgram >> sequence_ disposeTextures

    return (Renderable [(ForwardShadingStage, program)] vao render' dispose)

createRenderableBoxSet :: Float -> [V3 GLfloat] -> IO Renderable
createRenderableBoxSet edgeSize translations = do
    (vao, render, disposeBox) <- createTexturedBox edgeSize
    (textures, disposeTextures) <- unzip <$>
        mapMaybeM loadImage
            [ "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_COL_VAR1_1K.jpg"
            , "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_NRM_1K.jpg"
            , "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_REFL_1K.jpg"
            , "data/RustMixedOnPaint012_1k/RustMixedOnPaint012_GLOSS_1K.jpg"
            ]
        {-
        mapMaybeM loadImage
            [ "data/Bricks01_1k/Bricks01_COL_VAR1_1K.jpg"
            , "data/Bricks01_1k/Bricks01_NRM_1K.jpg"
            , "data/Bricks01_1k/Bricks01_REFL_1K.jpg"
            , "data/Bricks01_1k/Bricks01_GLOSS_1K.jpg"
            ]
        -}

    (program1, disposeProgram1) <- createProgramWithShaders' "shadow_vs.glsl" "shadow_fs.glsl"
    (program2, disposeProgram2) <- createProgramWithShaders' "box_vs.glsl" "box_fs.glsl"
    (program3, disposeProgram3) <- createProgramWithShaders' "box_vs.glsl" "box_deferred_fs.glsl"

    let programs =
            [ (ShadowMappingStage, program1)
            , (ForwardShadingStage, program2)
            , (DeferredShadingStage, program3)
            ]
        disposeAll = do
            disposeBox
            disposeProgram1
            disposeProgram2
            disposeProgram3
            sequence_ disposeTextures

    transformations <- mapM
        (newMatrix RowMajor . flattenMatrix . mkTransformation noRotation) translations
        :: IO [GLmatrix GLfloat]
    -- TODO Use instanciation instead.
    let render' p = forM_ transformations $ \transformation -> do
            setUniform p "transformation" transformation
            setUniform p "materialSpecularIntensity" (5 :: GLfloat)
            setUniform p "materialSpecularPower" (20 :: GLfloat)
            usingOrderedTextures p textures (render p)

    return (Renderable programs vao render' disposeAll)

createTransparentBox :: Float -> IO Renderable
createTransparentBox edgeSize = do
    (vao, render, dispose) <- createBox edgeSize
    (program, disposeProgram) <- createProgramWithShaders' "generic_vs.glsl" "box_fs.glsl"

    let -- rotation = axisAngle (V3 1 2 3) (pi / 5)
        translation = V3 (-1.5) (-0.5) 0.6
        t = mkTransformation noRotation translation

    transformation <- newMatrix RowMajor (flattenMatrix t) :: IO (GLmatrix GLfloat)

    let render' p = do
            setUniform p "transformation" transformation
            withState blend Enabled .
                withState blendFunc (SrcAlpha, OneMinusSrcAlpha) .
                withState depthMask Disabled .
                withState cullFace Nothing $ render p

    return (Renderable [(ForwardShadingStage, program)] vao render' (dispose >> disposeProgram))

createSkyBox :: IO Renderable
createSkyBox = do
    (vao, render, disposeSkyBox) <- createTexturedSkyBox far -- Using far is cheating here...
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

    let render' p =
            withState cullFace (Just Front) .
            withState depthMask Disabled .
            usingOrderedTextures program textures $ render p
        dispose = disposeSkyBox >> disposeProgram >> sequence_ disposeTextures

    return (Renderable [(ForwardShadingStage, program)] vao render' dispose)

createTesselatedPyramid :: IO Renderable
createTesselatedPyramid = do
    (vao, render, dispose) <- createPatchPyramid
    (program, disposeProgram) <- createProgramWithShaders
        [ ("triangle_vs.glsl", VertexShader)
        , ("triangle_cs.glsl", TessControlShader)
        , ("triangle_es.glsl", TessEvaluationShader)
        , ("triangle_fs.glsl", FragmentShader)
        ]

    let render' p = do
            patchVertices $= 3
            withState polygonMode (Line, Line) .
                withState cullFace Nothing $
                render p

    return (Renderable [(ForwardShadingStage, program)] vao render' (dispose >> disposeProgram))

createScreen :: IO Renderable
createScreen = do
    (vao, render, dispose) <- createSquare (0, 0) 1.95
    (program, disposeProgram) <- createProgramWithShaders' "screen_vs.glsl" "screen_fs.glsl"
    return (Renderable [(ForwardShadingStage, program)] vao render (dispose >> disposeProgram))

createDeferredScreen :: IO Renderable
createDeferredScreen = do
    (vao, render, dispose) <- createSquare (0, 0) 2
    (program, disposeProgram) <- createProgramWithShaders' "screen_vs.glsl" "deferred_screen_fs.glsl"
    return (Renderable [(ForwardShadingStage, program)] vao render (dispose >> disposeProgram))
